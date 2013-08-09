gem_group :development, :test do
    gem 'pry'
    gem 'pry-rails'
    gem 'rspec'
    gem 'rspec-rails'
    gem 'guard'
    gem 'guard-rspec'
    gem 'capybara'
end

gem_group :development do
    gem 'rack-mini-profiler'
end

# Remove the secret token from the tracking
run "echo 'config/initializers/secret_token.rb' >> .gitignore"
