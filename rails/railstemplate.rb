gem_group :development, :test do
    # Better testing dsl
    gem 'rspec'
    gem 'rspec-rails'
    gem 'database_cleaner'
    gem 'pry-rails'
    gem 'fuubar'

    # Integration tests with a real headless browser
    gem 'capybara'
end

# For encrypted passwords
gem 'bcrypt-ruby'

# Remove the secret token from the tracking
run "echo 'config/initializers/secret_token.rb' >> .gitignore"
run "echo 'config/database.yml' >> .gitignore"

# Set up foreman
run "echo 'web: bundle exec rails s -p $PORT' >> Procfile"
run "echo PORT=3000 >> .env"
run "echo '.env' >> .gitignore"
# disable output buffering
run "echo 'STDOUT.sync = true' >> config/environments/development.rb"

# Install rspec and capybara
generate "rspec:install"

inject_into_file "spec/spec_helper.rb", after: "require 'rspec/autorun'\n" do
  "require 'capybara/rspec'\n"
end

inject_into_file "spec/spec_helper.rb", after: "config.use_transactional_fixtures = true\n" do
  %Q(
  # Only allow expect syntax
  config.syntax = :expect
  )
end
