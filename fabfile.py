from fabric.api import run, env, cd, local

env.use_ssh_config = True
env.hosts = ['chain.astro', 'ngtshead.astro']

def sync():
    with cd("~/dotfiles"):
        run('git sync')
