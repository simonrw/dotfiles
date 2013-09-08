require_relative 'lib/oh_my_zsh'

$exclude_list = ['.git', 'osx', '.', '..', 'oh-my-zsh', 'lib']

# Every subdirectory below this one should contain extra objects either files or directories which will get linked into ~
# This function returns all of these
def get_to_links
    to_links = []

    dotfiles_dir = File.dirname(__FILE__)

    Dir.entries(dotfiles_dir).each do |filename|
        if File.directory? filename and not $exclude_list.include? filename
            subfiles = Dir.entries File.join dotfiles_dir, filename

            subfiles.each do |sfilename|
                if not sfilename.eql? '..' and not sfilename.eql? '.'
                    full_path = File.join dotfiles_dir, filename, sfilename
                    to_links << full_path
                end
            end

        end
    end

    to_links
end

# Returns the name of the link that will be created
def link_name(name)
    File.expand_path(File.join('~', '.' + File.basename(name)))
end

def add_links
    for name in get_to_links
        ln = link_name(name)
        begin
            File.symlink File.expand_path(name), ln
            puts "Linking #{ln}"
        rescue
            puts "Link #{ln} already found"
        end
    end
end

# Remove the links that would be installed by this script
def remove_links
    for name in get_to_links
        ln = link_name(name)

        if File.symlink? ln
            puts "Removing link #{ln}"
            File.unlink ln
        end
    end
end

module OS
    def OS.windows?
        (/cygwin|mswin|mingw|bccwin|wince|emx/ =~ RUBY_PLATFORM) != nil
    end

    def OS.mac?
        (/darwin/ =~ RUBY_PLATFORM) != nil
    end

    def OS.unix?
        !OS.windows?
    end

    def OS.linux?
        OS.unix? and not OS.mac?
    end
end

def source_osx_file(arg, fname)
    if arg
        system "source #{File.join File.dirname(__FILE__), 'osx', fname}"
    end
end

task :default do
    tasks = `rake --tasks`

    puts 'Tasks available:'
    puts tasks
end

desc 'Links the respective files into the correct places'
task :install, [:install_osx] do |t, args|
    add_links
    source_osx_file args[:install_osx], 'defaults.sh'
end

desc 'Removes any soft-links created by this script'
task :uninstall, [:uninstall_osx] do |t, args|
    remove_links
    source_osx_file args[:uninstall_osx], 'restore.sh'
end

desc 'Reinstalls directories'
task :reinstall do 
    Rake::Task["uninstall"].invoke
    Rake::Task["install"].invoke
end

desc 'Synchronises the repositories'
task :deploy do
    sh 'git sync'
end
