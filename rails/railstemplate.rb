gem_group :development, :test do
    # Better testing dsl
    gem 'rspec'
    gem 'rspec-rails'
    gem 'cucumber-rails', require: false
    gem 'database_cleaner'

    # Integration tests with a real headless browser
    gem 'capybara'
end

# For encrypted passwords
gem 'bcrypt-ruby'

# Remove the secret token from the tracking
run "echo 'config/initializers/secret_token.rb' >> .gitignore"
run "echo 'config/database.hml' >> .gitignore"

# Set up foreman
run "echo 'web: bundle exec rails s -p $PORT' >> Procfile"
run "echo PORT=3000 >> .env"
run "echo '.env' >> .gitignore"
# disable output buffering
run "echo 'STDOUT.sync = true' >> config/environments/development.rb"

git :init

# Install rspec and capybara
generate "rspec:install"
generate "cucumber:install"

inject_into_file "spec/spec_helper.rb", after: "require 'rspec/autorun'\n" do
  "require 'capybara/rspec'\n"
end
