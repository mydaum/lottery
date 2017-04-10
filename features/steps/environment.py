
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
