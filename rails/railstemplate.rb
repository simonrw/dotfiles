gem_group :development, :test do
    # Nice repl
    gem 'pry'
    gem 'pry-rails'

    # Better testing dsl
    gem 'rspec'
    gem 'rspec-rails'

    # Automated tasks
    gem 'guard'
    gem 'guard-rspec'

    # Integration tests with a real headless browser
    gem 'capybara'
end

# Foreman for running multiple tasks
gem 'foreman'

# For encrypted passwords
gem 'bcrypt-ruby'

# Remove the secret token from the tracking
run "echo 'config/initializers/secret_token.rb' >> .gitignore"

# Set up foreman
run "echo 'web: bundle exec rails s -p $PORT' >> Procfile"
run "echo PORT=3000 >> .env"
run "echo '.env' >> .gitignore"
# disable output buffering
run "echo 'STDOUT.sync = true' >> config/environments/development.rb"

git :init
