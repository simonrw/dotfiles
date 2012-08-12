#!/usr/bin/env python
# encoding: utf-8

'''
Install the soft links required for the dotfiles repository
'''

import os

EXCLUDE_LIST = ['.git', 'osx']


def get_to_links():
    '''
    Every subdirectory below this one should contain extra objects either files
    or directories which will get linked into ~

    This function returns all of these.
    '''
    to_links = []

    dotfiles_dir = os.path.dirname(__file__)

    # Was complaining if the current directory was being listed
    if not len(dotfiles_dir):
        dotfiles_dir = "."

    for name in os.listdir(dotfiles_dir):
        if os.path.isdir(name) and (name not in EXCLUDE_LIST):
            # Every subdirectory contained in the same directory as this file
            # and not the .git dir
            subfiles = os.listdir(os.path.join(dotfiles_dir, name))

            # Make sure the paths are joined
            to_links.extend([os.path.join(dotfiles_dir, name, s)
                for s in subfiles])

    return to_links


def link_name(name):
    '''
    Returns the name of the link that will be created
    '''
    return os.path.expanduser(os.path.join("~", "." + os.path.basename(name)))


def add_links():
    '''
    Add all of the required links
    '''
    for name in get_to_links():
        ln = link_name(name)
        if not os.path.islink(ln):
            print "Linking %s" % ln
            try:
                os.symlink(os.path.abspath(name), ln)
            except OSError:
                print "Warning: Cannot link {:s}".format(name)
        else:
            print "Link %s already found" % ln


def remove_links():
    '''
    Remove the links that would be installed by this script
    '''
    for name in get_to_links():
        ln = link_name(name)

        if os.path.islink(ln):
            print "Removing link %s" % ln
            os.remove(ln)


def ostype():
    '''
    Returns the operating system type
    '''
    return os.uname()[0]


def main(args):
    is_mac = (ostype() == "Darwin")
    if args.command == 'install':
        add_links()

        # Handle setting up the defaults
        if is_mac:
            ans = raw_input("This system is OSX, do you want to setup the "
                    "defaults? [Y/N]\n> ")
            if ans.upper() in ['Y', ]:
                os.system("source %s" %
                        (os.path.join(os.path.dirname(__file__), "osx",
                            "defaults.sh"), ))

    else:
        remove_links()
        if is_mac:
            ans = raw_input("This system is OSX, do you want to remove the "
            "custom defaults? [Y/N]\n> ")
            if ans.upper() in ['Y', ]:
                os.system("source %s" %
                        (os.path.join(os.path.dirname(__file__), "osx",
                            "restore.sh"), ))


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()

    parser.add_argument("command", help="Command to run", choices=['install',
        'uninstall', ])
    main(parser.parse_args())
