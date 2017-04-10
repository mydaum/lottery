import behave as b
import subprocess as sp
import unirest as uni


class Lottery:

    def __init__(self, port):
        self._port = port

    @property
    def port(self):
        return self._port


@b.given('that I have started lottery')
def step_start_application(context):
    port = 8888
    with sp.Popen(['lottery', '--port {}'.format(port)]) as proc:
        context.daemons.append(proc)
        context.lottery = Lottery(port=port)


@b.when('I create a lottery')
def step_create_lottery(context, name):
    port = context.lottery.port
    r = uni.post('http://localhost:{}/api/1/lottery/create'.format(port))
    context.response = r


@b.then('I get a (?P<code>\d+) response code')
def step_get_response_code(context, code):
    assert context.reponse.code == code
    if code == 204:
        assert len(context.reponse.body) == 0


@b.then('I get a URL')
def step_get_url(context):
    assert 'url' in context.response.body


@b.then('I get a deletion token')
def step_deletion_token(context):
    assert 'deletion_token' in context.response.body


@b.given('that a lottery has been created')
def step_lottery_has_been_created(context, id):
    context.exectute_steps('''
        I create a lottery
    ''')


@b.when('I delete that lottery')
def step_delete_lottery(context, name):
    port = context.lottery.port
    url = context.response.body['url']
    deletion_token = context.response.body['deletion_token']
    r = uni.delete(url='http://localhost:{}/{}'.format(port, url),
                   params={'deletion_token': deletion_token})
    context.response = r


@b.when('I delete that lottery using a bad token')
def step_delete_lottery_with_bad_token(context, name):
    port = context.lottery.port
    url = context.response.body['url']
    r = uni.delete(url='http://localhost:{}/{}'.format(port, url),
                   params={'deletion_token': 'bad_token'})
    context.response = r


@b.when('I delete a non-existing lottery')
def step_delete_non_existing_lottery(context, name):
    port = context.lottery.port
    r = uni.delete(url='http://localhost:{}/non-existing-lottery'.format(port),
                   params={'deletion_token': 'test_token'})
    context.response = r


@b.when('I delete a non-existing lottery using a bad token')
def step_delete_non_existing_lottery_with_bad_token(context, name):
    port = context.lottery.port
    r = uni.delete(url='http://localhost:{}/non-existing-lottery'.format(port),
                   params={'deletion_token': 'bad_token'})
    context.response = r
