from fabric.api import *

env.use_ssh_config = True


def lrun(*args, **kwargs):
    func = local if env.host == 'localhost' else run
    return func(*args, **kwargs)


def sync_repos():
    with cd('~/dotfiles'):
        lrun('git fetch origin')
        with settings(warn_only=True):
            result = lrun('git merge --ff-only origin/master')
            if result.failed:
                open_shell()


def sync_vim_plugins():
    with hide('stdout', 'stderr'):
        lrun('vim +PlugInstall +qa')


@task
def update():
    sync_repos()
    sync_vim_plugins()
