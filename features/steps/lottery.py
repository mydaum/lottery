from behave import given
import subprocess as sp
import unirest as uni


def before_scenario(context, scenario):
    context.daemons = []
    context.lottery = None


def after_scenario(context, scenario):
    for d in context.daemons:
        d.terminate()
        try:
            d.wait(60)
        except sp.TimeoutExpired:
            d.kill()
            d.wait(60)


class Lottery:

    def __init__(self, port):
        self._port = port

    @property
    def port(self):
        return self._port


@given('that I have started lottery')
def step_impl(context):
    port = 8888
    with sp.Popen(['lottery', '--port {}'.format(port)]) as proc:
        context.daemons.append(proc)
        context.lottery = Lottery(port=port)


@given('I create a lottery')
def step_impl(context, name):
    port = context.lottery.port
    r = uni.post('http://localhost:{}/api/1/lottery/create'.format(port))
    context.response = r


@given('I get a (?P<code>\d+) response code')
def step_impl(context, code):
    assert context.reponse.code == code
    if code == 204:
        assert len(context.reponse.body) == 0


@given('I get a URL')
def step_impl(context):
    assert 'url' in context.response.body


@given('I get a deletion token')
def step_impl(context):
    assert 'deletion_token' in context.response.body


@given('a lottery has been created')
def step_impl(context, id):
    context.exectute_steps('''
        I create a lottery
    ''')


@given('I delete a lottery')
def step_impl(context, name):
    port = context.lottery.port
    url = context.response.body['url']
    deletion_token = context.response.body['deletion_token']
    r = uni.delete(url='http://localhost:{}/{}'.format(port, url),
                   params={'deletion_token': deletion_token})
    context.response = r


@given('I delete a non-existing lottery')
def step_impl(context, name):
    port = context.lottery.port
    r = uni.delete(url='http://localhost:{}/non-existing-lottery'.format(port),
                   params={'deletion_token': 'test_token'})
    context.response = r
