from fabric.api import *

env.use_ssh_config = True
env.forward_agent = True


def lrun(*args, **kwargs):
    func = local if env.host == 'localhost' else run
    return func(*args, **kwargs)


@task
def sync_repos():
    with cd('~/dotfiles'):
        lrun('git fetch origin')
        with settings(warn_only=True):
            lrun('git stash -q')
            result = lrun('git merge --ff-only origin/master')
            if result.failed:
                open_shell()
            lrun('git stash pop -q 2>/dev/null || true')


@task
def sync_vim_plugins():
    with hide('stdout', 'stderr'):
        lrun('~/.bin/vpi')
        lrun('vim +PlugUpdate +qa')
        lrun('vim +PlugClean +qa')


@task
@serial
def push_changes():
    local('git push origin master')


@task(default=True)
def update():
    push_changes()
    sync_repos()
    sync_vim_plugins()
