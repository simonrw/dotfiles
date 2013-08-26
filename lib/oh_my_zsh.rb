class OhMyZSH
    def initialize(options = {})
        @location = options['location'] || File.join(ENV['HOME'], '.oh-my-zsh')
        @file_directory = options['directory'] || File.join(File.dirname(__FILE__), '..')
        @repository = options['repository'] || "git://github.com/robbyrussell/oh-my-zsh.git"
    end

    def install
        (exists? || fetch) && link_custom_contents
    end

    def uninstall
        exists? && remove
    end

private

    def exists?
        File.exist? @location
    end

    def fetch
        system %Q{git clone #{@repository} #{@location}}
    end

    def remove
        system %Q{rm -rf "#{@location}"}
    end

    def source_location
        File.expand_path(File.join(@file_directory, 'oh-my-zsh', 'custom'))
    end

    def custom_location
        File.expand_path(File.join(@location, 'custom'))
    end

    def update_custom
        remove_custom && link_custom
    end

    def remove_custom
        system %Q{rm -rf "#{custom_location}"}
    end

    def link_custom_contents
        system %Q{ln -sv #{source_location} #{custom_location}}
    end
end
