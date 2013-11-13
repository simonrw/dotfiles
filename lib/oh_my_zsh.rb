class OhMyZSH
    def initialize(options = {})
        @location = options['location'] || File.join(ENV['HOME'], '.oh-my-zsh')
        @file_directory = options['directory'] || File.join(File.dirname(__FILE__), '..')
        @repository = options['repository'] || "git://github.com/robbyrussell/oh-my-zsh.git"
    end

    def install
        (exists? || fetch) && link_custom_contents && link_theme_contents
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

    def theme_source_location
        File.expand_path(File.join(@file_directory, 'oh-my-zsh', 'themes'))
    end

    def custom_location
        File.expand_path(File.join(@location, 'custom'))
    end

    def themes_location
        File.expand_path(File.join(@location, 'themes'))
    end

    def custom_contents
        Dir["#{source_location}/*"]
    end

    def themes_contents
        Dir["#{theme_source_location}/*"]
    end

    def new_path(path)
        if path =~ /custom/
            File.join(custom_location, File.basename(path))
        elsif path =~ /theme/
            File.join(themes_location, File.basename(path))
        else
            raise "Cannot link #{path}"
        end
    end

    def link_custom_contents
        custom_contents.each do |source|
            begin
                puts "Linking #{source} -> #{new_path(source)}"
                File.symlink(source, new_path(source))
            rescue
                puts "File #{new_path(source)} already found"
            end
        end
    end

    def link_theme_contents
        themes_contents.each do |source|
            begin
                puts "Linking #{source} -> #{new_path(source)}"
                File.symlink(source, new_path(source))
            rescue
                puts "File #{new_path(source)} already found"
            end
        end
    end
end
