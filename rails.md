This tutorial is based on the excellent book: "Ruby on Rails Tutorial
- Learn Rails by Example", by Michael Hartl.  I'll abbreviate this
with the ackronym: RORT.

# Setup testing

Add rspec, guard, livereload, spork, capybara, etc... to `Gemfile`

```ruby
group :development, :test do
  gem 'rspec-rails', '2.10.0'
  gem 'guard-rspec' # 0.5.5
  gem 'guard-livereload'
end
group :test do
  gem 'rspec-rails', '2.10.0'
  gem 'capybara', '1.1.2'
  gem 'rb-inotify', '0.8.8'
  gem 'libnotify', '0.5.9'
  gem 'guard-spork', '0.3.2'
  gem 'spork', '0.9.0'
end      
```

I've also setup notifications on my arch linux system using `twmn`.
See (RORT, 105) for advice on how to setup notifications for your
system. 

# Create a simple page

## Routing

File `config/routes.rb` insert following line:

    get 'pages/index'
    
## Controller
 
Create file: `app/controllers/pages_controller.rb` with contents:

```ruby
class PagesController < ApplicationController
  def index
  end
end
```

## View

Create file `app/views/pages/index.html.erb`, with the following contents:

```html
<%
name = "Fenton"
%>
<h1>Hello <%=name%></h1>
```

## Test

    http://0.0.0.0:3000/pages/index
    
Okay now lets setup some testing.

    $ mkdir -p spec/requests

Have the following as the contents for the file:
`spec/spec_helper.rb`:

```ruby
require 'rubygems'
require 'spork'
Spork.prefork do
  ENV["RAILS_ENV"] ||= 'test'
  require File.expand_path("../../config/environment", __FILE__)
  require 'rspec/rails'
  require 'rspec/autorun'
  require 'capybara'
  Dir[Rails.root.join("spec/support/**/*.rb")].each {|f| require f}
  RSpec.configure do |config|
    config.mock_with :rspec
    config.fixture_path = "#{::Rails.root}/spec/fixtures"
    config.use_transactional_fixtures = true
    config.infer_base_class_for_anonymous_controllers = false
    config.include Capybara::DSL
  end
end
Spork.each_run do
end
```

Create file: `spec/requests/pages_spec.rb` with contents:

```ruby
require 'spec_helper'
describe "Pages" do
  describe "GET /pages/index" do
    it "should have Fenton in the body" do
      visit '/pages/index'
      page.should have_selector('h1', :text => "Fenton")
    end
  end
end
```

Now run the test with:

    $ bundle exec rspec spec/requests/pages_spec.rb

# Automated watching tests: Guard, Spork

Guard will watch for file changes and rerun the required tests.  Spork
speeds up the loading of the Rails environment by preloading it.  This
helps to keep our test times down.

In file: `Guardfile` put:

```ruby
require 'active_support/core_ext'
Guardfile
guard 'rspec', :version => 2, :all_after_pass => false, :cli => '--drb' do
  watch(%r{^app/controllers/(.+)_(controller)\.rb$}) do |m|
    ["spec/routing/#{m[1]}_routing_spec.rb",
     "spec/#{m[2]}s/#{m[1]}_#{m[2]}_spec.rb",
     "spec/acceptance/#{m[1]}_spec.rb",
     "spec/requests/#{m[1]}_spec.rb",
     "spec/requests/pages_spec.rb"]
  end
  watch(%r{^app/views/(.+)/}) do |m|
    "spec/requests/#{m[1]}_spec.rb"
  end
end
guard 'spork', :rspec_env => { 'RAILS_ENV' => 'test' }, test_unit: false, cucumber: false do
  watch('config/application.rb')
  watch('config/environment.rb')
  watch(%r{^config/environments/.+\.rb$})
  watch(%r{^config/initializers/.+\.rb$})
  watch('Gemfile')
  watch('Gemfile.lock')
  watch('spec/spec_helper.rb')
  watch('test/test_helper.rb')
  watch('spec/support/')
end
```

Now in a terminal that you keep running do:

    $ bundle exec guard
    
Now after you let that settle down, lets update the view file:
`app/views/pages/index.html.erb`, to inject an error.  Change `Fenton`
to `Fentonn` and see that you get an auto notification about the
error instantly.

# Database Modelling

Lets do some simple DB modelling.

    $ rails generate model User name:string email:string

The above will create some files.  Have a look at them.  Now actually
create the table with:

    $ bundle exec rake db:migrate

The first time `db:migrate` is run, it creates a file called
`db/development.sqlite3`, which is an SQLite database.

Open a rails console:

    $ rails console --sandbox


----------

# Rails testing

When test web pages put test files in: `spec/requests`.  When testing
models put files in: `spec/model`.

## View Testing

First lets look at the view test: `spec/requests/pages_spec.rb` with
contents:

```ruby
require 'spec_helper'
describe "Pages" do
  describe "GET /pages/index" do
    it "should have Fenton in the body" do
      visit '/pages/index'
      page.should have_selector('h1', :text => "Fenton")
    end
  end
end
```

To go to a page use: `visit` as in:

    visit '/pages/index'
    
In our `config/routes` we have:

    get 'pages/index'
    
and we have a controller: `app/controllers/pages_controller.rb` with
an empty function `index` defined, which will eventually automatically
route to: `app/views/pages/index.html.erb`.

This will create a `page` variable which has a `should` method on it.  

    have_selector( 'h1', :text => "Hello" )
    
Means the page should have an `<h1>` tag in it, that has text "hello"
somewhere in the tag.

    page.should have_content('Sample App')

Has somewhere in the page the content: "Sample App".

Note you can also use the function `should_not`.

We can eliminate these sources of duplication by telling RSpec that
page is the subject of the tests using

    subject { page }
    it { should have_selector ...
    
You can run some code before a bunch of selectors using:

    before { visit contact_path }
    
In our `routes.rb` we can put lines like:

    match '/contact', to: 'static_pages#contact'

which will give us a variable like `contact_path`, which we can use
with `visit`

    click_link "About"
    
Will visit a like with the `id` = "About" ???

```ruby
describe User do
  pending "add some examples to (or delete) #{__FILE__}"
end
```

`pending` is like a TODO: note, for you to come back later and fill it in.

Fill in a form

```ruby
visit signup_path
fill_in "Name", with: "Example User"
...
click_button "Create my account"
```

this will put "Example User" in input box with id?/name? = `Name`.

When submitting a form we expect something to be updated in the DB, so
we can use the `count` method available on every Active Record object.

    expect { click_button "Create my account" }.not_to change(User, :count)

The `change` method, which takes as arguments an object and a symbol
and then calculates the result of calling that symbol as a method on
the object both before and after the block.

```ruby
expect do
  click_button "Create my account"
end.to change(User, :count).by(1)
```

Or as above, change the count by 1.







----

# form_for

When creating a new resource, user, order, etc..., we use the `new`
action/method.  When we want to save the user, we use the `create`
action/method. 

In the new method we create a blank `user` (say), like so:

    @user = User.new
    
Then in the corresponding: `app/views/users/new.html.erb` we use
`form_for`: 

`form_for` takes an `ActiveRecord` and constructs a form from it's
attributes. 

```erb
<%= form_for(@user) do |f| %>
  <%= f.label :name %>
  <%= f.text_field :name %>
  <%= f.label :email %>
  <%= f.text_field :email %>
  <%= f.label :password %>
  <%= f.password_field :password %>
  <%= f.label :password_confirmation, "Confirmation" %>
  <%= f.password_field :password_confirmation %>
  <%= f.submit "Create my account", class: "btn btn-large btn-primary" %>
<% end %>
```

Submitting this form will go to the `create` action/method where we'd
use code like:

```ruby
def create 
  @user = User.new(params[:user])
  if @user.save
    # handle successful save
  else
    render 'new'
  end
end
```

here `params[:user]` expands to a hash of the user attributes just as
required by `User.new`

`render 'new'` will go _back_ to the create new user page on any
errors.

Take note of the created html:

```html
<input id="user_email" name="user[email]" size="30" type="text" />
```

When this form gets created, the `params` hash, add another hash with
key `user`.  This user hashes keys come from the `name` attributes of
the input boxes, so `user[email]` is a key on the `user` hash.

Although the hash keys appear as strings in the debug output,
internally Rails uses symbols, so that `params[:user]` is the hash of
`user` attributes.

## Errors

To handle errors we put in a *partial* like so:

```erb
<%= form_for(@user) do |f| %>
  <%= render 'shared/error_messages' %>
  ...
<% end %>
```






----

# Model Testing

We have create a `User` model.  So we create a test:
`spec/model/user_spec.rb`.

```ruby
describe User do
  before { @user = User.new(name: "Example User", email: "user@example.com") }
  subject { @user }
  it { should respond_to(:name) }
  it { should respond_to(:email) }
end
```

Basically tests that there are attributes email and name defined like: 

```ruby
class User
  attr_accessor :name, :email
end
```

The tests themselves rely on the boolean convention used by RSpec: the
code

    @user.respond_to?(:name)

can be tested using the RSpec code

    @user.should respond_to(:name)


Models that extend `ActiveRecord` get a method `valid?` that returns
`false` when the object fails one or more validations, and `true` when
all validations pass.

In this case, we can test the result of calling

    @user.valid?

with

    @user.should be_valid

As before, `subject { @user }` lets us leave off `@user`, yielding

    it { should be_valid }

We could make a rule that if a user doesn't have an email address then
it isn't valid with the following test:

```ruby
describe "when email is not present" do
  before { @user.email = " " }
  it { should_not be_valid }
end
```

Test length validation:

```ruby
describe "when name is too long" do
  before { @user.name = "a" * 51 }
  it { should_not be_valid }
end
```

Format tests:

```ruby
describe "when email format is invalid" do
  it "should be invalid" do
    addresses = %w[user@foo,com user_at_foo.org example.user@foo. foo@bar_baz.com foo@bar+baz.com]
    addresses.each do |invalid_address|
      @user.email = invalid_address
      @user.should_not be_valid
    end
  end
end
```

Uniqueness tests:

```ruby
describe "when email address is already taken" do
  before do
    user_with_same_email = @user.dup
    user_with_same_email.save
  end
  it { should_not be_valid }
end
```

the `dup` method makes a duplicate, _in memory_ only.  Note when a
record does not save, the `valid?` method should return false.

Case insensitive uniqueness tests:

Since emails are case insensitive we need this for emails:

```ruby
describe "when email address is already taken" do
  before do
    user_with_same_email = @user.dup
    user_with_same_email.email = @user.email.upcase
    user_with_same_email.save
  end
  it { should_not be_valid }
end
```

Testing Secure Password:

You need a column `password_digest` where the hashed password will
go.

    it { should respond_to(:password_digest) }

Also add `password` and `password_confirmation` tests:

```ruby
before do
  @user = User.new(name: "Example User", email: "user@example.com", password: "foobar", password_confirmation: "foobar")
end
subject { @user }
  it { should respond_to(:password) }
  it { should respond_to(:password_confirmation) }
  it { should be_valid }
```

Test passwords are not blank:

```ruby
describe "when password is not present" do
  before { @user.password = @user.password_confirmation = " " }
  it { should_not be_valid }
end
```

Test mismatch:

```ruby
describe "when password doesn't match confirmation" do
  before { @user.password_confirmation = "mismatch" }
  it { should_not be_valid }
end
```

Also check if confirmation is `nil`.

```ruby
describe "when password confirmation is nil" do
  before { @user.password_confirmation = nil }
  it { should_not be_valid }
end
```

`let`

We use `let` to define a variable in rspec tests.

```ruby
let (:my_var) { "abc123" }
```

The keyword is assigned the results of the following block.  In the
subsequent code *DONT* use the keyword, drop the preceeding `:`, and
use like a normal variable.

## Authenticating Users Tests:

We start by requiring a User object to respond to authenticate:

    it { should respond_to(:authenticate)}

We then cover the two cases of password match and mismatch:

```ruby
describe "return value of authenticate method" do
  before { @user.save }
  let(:found_user) { User.find_by_email(@user.email) }
  describe "with valid password" do
    it { should == found_user.authenticate(@user.password) }
  end
  describe "with invalid password" do
    let(:user_for_invalid_password) { found_user.authenticate("invalid") }
    it { should_not == user_for_invalid_password }
    specify { user_for_invalid_password.should be_false }
  end
end
```    

`specify` method. This is just a synonym for `it`, and can be used when
writing `it` would sound unnatural. In this case, it sounds good to say
"it [i.e., the user] should not equal wrong user", but it sounds
strange to say "user: user with invalid password should be false";
saying "specify: user with invalid password should be false" sounds
better.

----

# View Testing

Often we'll want to test pages that show info from the DB.  We auto
add info to the DB by using factories.  Edit `Gemfile`:

```ruby
group :test do
  ...
  gem 'factory_girl_rails', '1.4.0'
end
```

in file: `spec/factories.rb`, put:

```ruby
FactoryGirl.define do
  factory :user do
    name "Michael Hartl"
    email "michael@example.com"
    password "foobar"
    password_confirmation "foobar"
  end
end
```

By passing the symbol `:user` to the factory command, we tell Factory
Girl that the subsequent definition is for a `User` model object.

Update `spec/requests/user_pages_spec.rb`, with:

```ruby
describe "profile page" do
  let(:user) { FactoryGirl.create(:user) }
  before { visit user_path(user) }
  it { should have_selector('h1', text: user.name) }
  it { should have_selector('title', text: user.name) }
end
```

----------

# Modelling Validations

Require the presence of `name` and `email`:

```ruby
class User < ActiveRecord::Base
  attr_accessible :name, :email
  validates :name, presence: true
  validates :email, presence: true
end
```

Length validations:

    validates :name, presence: true, length: { maximum: 50 }

Format validation:

```ruby
VALID_EMAIL_REGEX = /\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/i
validates :email, presence: true, format: { with: VALID_EMAIL_REGEX }
```

Uniqueness Validations:

Uniqueness is different from previous validations as it needs to be
stored into the database to be tested for.  This is substantially more
difficult for the following reasons:

    validates :email, uniqueness: true

Email addresses are case-insensitive—foo@bar.com goes to the same
place as FOO@BAR.COM or FoO@BAr.coM —so our validation should cover
this case as well.

    validates email:, uniqueness: { case_sensitive: false }
    
Still duplicate emails can get into the DB like so:

* User clicks create *twice* by accident.

* 1st `User` gets created in memory, passes validations, then 2nd
  `User` gets created *in memory* and passes validation.  Finally both
  get persisted to DB.

To fix this we need to put an index onto the email column of the DB,
and require that it be unique too.

We can do this like so:

    $ rails generate migration add_index_to_users_email

Here is the file:
`db/migrate/[timestamp]_add_index_to_users_email.rb`:

```ruby
class AddIndexToUsersEmail < ActiveRecord::Migration
  def change
    add_index :users, :email, unique: true
  end
end
```

Migrate the DB:

    $ bundle exec rake db:migrate

Unfortunately, there’s one more change we need to make to be assured
of email uniqueness, which is to make sure that the email address is
all lower-case before it gets saved to the database. The reason is
that not all database adapters use case-sensitive indices.

The way to do this is with a callback, which is a method that gets
invoked at a particular point in the lifetime of an Active Record
object.

In the present case, we’ll use a `before_save` *callback* to force Rails
to downcase the email attribute before saving the user to the
database, as shown below in the file: `app/models/user.rb` below:

```ruby
class User < ActiveRecord::Base
  attr_accessible :name, :email
  before_save { |user| user.email = email.downcase }
  ...
end
```

# Making a secure password

Enable gem: `gem 'bcrypt-ruby', '3.0.1'`.

    $ rails generate migration add_password_digest_to_users password_digest:string

We can choose any migration name we want, but it’s convenient to end
the name with `_to_users`, since in this case Rails automatically
constructs a migration to add columns to the users table.

file: `db/migrate/[ts]_add_password_digest_to_users.rb`

```ruby
class AddPasswordDigestToUsers < ActiveRecord::Migration
  def change
    add_column :users, :password_digest, :string
  end
end
```

In the user model add:

```ruby
attr_accessible :name, :email, :password, :password_confirmation
has_secure_password
validates :password, presence: true, length: { minimum: 6 }
validates :password_confirmation, presence: true
```

The magic is done in `has_secure_password` in rails >= 3.1


----

# Test Data

`Gemfile`:

    gem 'faker', '1.0.1'
    
`lib/tasks/sample_data.rake`

```ruby
namespace :db do
  desc "Fill database with sample data"
  task populate: :environment do
    User.create!(name: "Example User",
                 email: "example@railstutorial.org",
                 password: "foobar",
                 password_confirmation: "foobar")
    15.times do |n|
      name = Faker::Name.name
      email = "example-#{n+1}@railstutorial.org"
      password = "password"
      User.create!(name: name,
                   email: email,
                   password: password,
                   password_confirmation: password)
    end
  end
end
```

Run it:

```bash
$ bundle exec rake db:reset
$ bundle exec rake db:populate
$ bundle exec rake db:test:prepare
```

## Factory Girl Test Data

`spec/factories.rb`:

```ruby
FactoryGirl.define do
  factory :user do
    sequence(:name) { |n| "Person #{n}" }
    sequence(:email) { |n| "person_#{n}@example.com"}
    password "foobar"
    password_confirmation "foobar"
  end
end
```

Apply with:

```ruby
before(:all) { 30.times { FactoryGirl.create(:user) } }
after(:all) { User.delete_all }
```




# Pagination

`Gemfile`:

```ruby
gem 'will_paginate', '3.0.3'
gem 'bootstrap-will_paginate', '0.0.6'
```



----

# REST

Three items: verb, resource, id.

4 verbs: get(read), put(update), post(create), delete

example verb, resource name and unique id: `get: /users/1`

Here the show action is implicit in the type of request—when Rails’
REST features are activated, GET requests are automatically handled by
the show action.

We can get the REST-style URI to work by adding a single line to our
routes file: `config/routes.rb`:

    resources :users
    
REST Table

HTTP Req. | URI           | Action  | Named Route          | Purpose
--------- | ------------- | ------  | -------------------- | -----------------------------
Get       | /users        | index   | users_path           | page to list all users
Get       | /users/1      | show    | user_path(user)      | page to show a user
Get       | /users/new    | new     | new_user_path        | page to make new user
Post      | /users        | create  | users_path           | create a new user 
Get       | /users/1/edit | edit    | edit_user_path(user) | page to edit user with id 1
Put       | /users/1      | update  | user_path(user)      | update user
Delete    | /users/1      | destroy | user_path(user)      | delete user



----

# Authorization

To (say) edit the user information corresponding to user with id: 1,
we'd use the URL: `/users/1/edit` which routes us through the edit
action on the `user` controller like so:

```ruby
def edit
  @user = User.find(params[:id])
end
```

This takes us to: `app/views/users/edit.html.erb` with the values
filled in.

## Sign-In Helper

`spec/requests/authentication_pages_spec.rb`

```ruby
require 'spec_helper'
describe "Authentication" do
  describe "with valid information" do
    let(:user) { FactoryGirl.create(:user) }
    before { sign_in user }
    it { should have_selector('title', text: user.name) }
    it { should have_link('Profile', href: user_path(user)) }
    it { should have_link('Settings', href: edit_user_path(user)) }
    it { should have_link('Sign out', href: signout_path) }
    it { should_not have_link('Sign in', href: signin_path) }
  end
end
```

`spec/support/utilities.rb`

```ruby
def sign_in(user)
  visit signin_path
  fill_in "Email",
  with: user.email
  fill_in "Password", with: user.password
  click_button "Sign in"
  # Sign in when not using Capybara as well.
  cookies[:remember_token] = user.remember_token
end
```















## Create DB

    $ rake db:create





## Install a javascript runtime

    $ gem install therubyracer

In your `Gemfile` put:

    $ gem 'therubyracer', require: "v8"

and run

    $ bundle install


# Behaviour Driven Development Rails

[ref][testing]

at the end of file: `config/environments/test/rb`, put:

```ruby
config.gem "rspec", :lib => false, :version => ">=1.2.2"
config.gem "rspec-rails", :lib => false, :version => ">=1.2.2"
config.gem "webrat", :lib => false, :version => ">=0.4.3"
config.gem "cucumber", :lib => false, :version => ">=0.2.2"
```

at the end of file: `Gemfile`, put:

```ruby
gem 'cucumber', '>= 0.2.2'
gem 'webrat', '>= 0.4.3'
gem 'rspec-rails', '>= 1.2.2'
gem 'rspec', '>= 1.2.2'
```

----

# Rails Console 

Printing out an object, (`ap` short for awesome print method):

```
$ bundle exec rails console
> require 'awesome_print'
> a = User.new
> ap a
```

## Pry Rails Console

`Gemfile`

    gem 'pry-rails', :group => :development

```
> show-routes
> show-models
```

Command                | Effect
-----------            | --------------------------------
ls -i                  | list instance variables
ls -Mp --grep ^pa      | show a list of all private instance methods (in scope) that begin with 'pa'
nesting                | show inheritance stack


* show methods source

Enter the Pry class, list the instance methods beginning with 're' and
display the source code for the rep method:

```
pry(main)> cd Pry
pry(Pry)> ls -M --grep re
Pry#methods: re  readline  refresh  rep  repl  repl_epilogue  repl_prologue  retrieve_line
pry(Pry):1> show-method rep -l
```

* show method documentation

Enter the `User` class, find instance methods beginning with 'up',
show documentation associated with method `update-attributes`

```
pry(main)> cd User
pry(User):1> ls -M --grep up
pry(User):1> show-doc update_attribute
```

The number after the : in the pry prompt indicates the nesting level.

----

# Basic Rails

[ref][tut1]

# Install rails

    $ gem install rails

# Create new rails app

Create an application called `catdb`

    $ rails new catdb
    
## Start it up!

    $ rails server
    
Test by navigating to:

    $ http://127.0.0.1:3000/




[testing]: http://railscasts.com/episodes/155-beginning-with-cucumber?view=asciicast
[tut1]: http://guides.rubyonrails.org/getting_started.html

