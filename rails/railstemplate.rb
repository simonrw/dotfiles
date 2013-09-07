gem_group :development, :test do
    # Better testing dsl
    gem 'rspec'
    gem 'rspec-rails'
    gem 'cucumber-rails'

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
