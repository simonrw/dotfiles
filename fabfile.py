from fabric.api import *

env.use_ssh_config = True


def lrun(*args, **kwargs):
    func = local if env.host == 'localhost' else run
    return func(*args, **kwargs)


@task
def sync_repos():
    with cd('~/dotfiles'):
        lrun('git fetch origin')
        with settings(warn_only=True):
            result = lrun('git merge --ff-only origin/master')
            if result.failed:
                open_shell()


@task
def sync_vim_plugins():
    with hide('stdout', 'stderr'):
        lrun('~/.bin/vpi')
        lrun('vim +PlugUpdate +qa')


@task
def push_changes():
    local('git push origin master')


@task(default=True)
def update():
    push_changes()
    sync_repos()
    sync_vim_plugins()
