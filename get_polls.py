import requests
import json
from time import sleep
"""
Get national polls
Get swing state polls
Do a regression to find a national trendline
"""

class ApiGetter(object):
    POLLS_PATH = 'polls.json'
    STATES = ['US','AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA',
              'HI','ID','IL','IN','IA','KS','KY','LA','ME','MD',
              'MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ',
              'NM','NY','NC','ND','OH','OK','OR','PA','RI','SC',
              'SD','TN','TX','UT','VT','VA','WA','WV','WI','WY']

    def __init__(self):
        pass

    def get_polls(self):
        with open(self.POLLS_PATH, 'a') as f:
            for state in self.STATES:
                page = 1
                while True:
                    s = self.call_api(state, page, '2016-president')
                    if s == '[]':
                        break
                    page += 1
                    for result in self.parse_api_output(s):
                        to_write = json.dumps(result)
                        f.write(to_write + '\n')
                        print(to_write)
                    print('Page {}. Sleeping for 2 seconds.'.format(page))
                    sleep(2)
                sleep(2)

    def call_api(self, state, page, topic):
        api_str = ('http://elections.huffingtonpost.com/'
                   'pollster/api/polls.json?'
                   'state={}&'
                   'page={}&'
                   'topic={}').format(state, page, topic)
        print('Making call {}.'.format(api_str))
        r = requests.get(api_str)
        return r.text

    @staticmethod
    def parse_api_output(api_output_str):
        """
        Give this the raw api output
        Yields results
        """
        token = ''
        j = json.loads(api_output_str)
        for poll in j:
            poll_dict = {
                'start_date': poll['start_date'],
                'end_date': poll['end_date'],
                'pollster': poll['pollster'],
            }
            for question in poll['questions']:
                if question['topic'] == '2016-president':
                    state = question.get('state')
                    poll_dict['state'] = state
                    for subpop in question['subpopulations']:
                        if subpop['name'] == 'Likely Voters':
                            poll_dict['observations'] = subpop['observations']
                            for response in subpop['responses']:
                                party = response['party']
                                value = response['value']
                                if party in {'Dem', 'Rep'}:
                                    poll_dict[party] = value
            yield poll_dict


if __name__ == '__main__':
    a = ApiGetter()
    a.get_polls()
