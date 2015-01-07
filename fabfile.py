from fabric.api import *

env.use_ssh_config = True


@task
def update():
    with cd('~/dotfiles'):
        run('git fetch origin')
        with settings(warn_only=True):
            result = run('git merge --ff-only origin/master')
            if result.failed:
                open_shell()
