from fabric.api import *

env.use_ssh_config = True
env.forward_agent = True


def lrun(*args, **kwargs):
    func = local if env.host == 'localhost' else run
    return func(*args, **kwargs)


@task
def sync_repos(remote_name='origin', branch_name='master'):
    with cd('~/dotfiles'):
        lrun('git fetch {}'.format(remote_name))
        with settings(warn_only=True):
            lrun('git stash -q')
            lrun('git checkout {}'.format(branch_name))
            result = lrun('git merge --ff-only {}/{}'.format(
                remote_name, branch_name
            ))
            if result.failed:
                open_shell()
            lrun('git stash pop -q 2>/dev/null || true')


@task
def sync_vim_plugins():
    with hide('stdout', 'stderr'):
        lrun('~/.bin/vpi')
        lrun('vim +PlugUpdate! +qa')
        lrun('vim +PlugClean +qa')


@task
@serial
def push_changes(remote_name='origin', branch_name='master'):
    local('git push {} {}'.format(remote_name, branch_name))


@task(default=True)
def update(remote_name='origin', branch_name='master'):
    push_changes(remote_name=remote_name, branch_name=branch_name)
    sync_repos(remote_name=remote_name, branch_name=branch_name)
    sync_vim_plugins()
