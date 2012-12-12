# Business Domain

Ad's have an image.  But one image may belong to many ads.

```ruby
class Ad < ActiveRecord::Base
  belongs_to :image
end  
class Image < ActiveRecord::Base
  has_many :ads
end
```

here is the right create sql for it:

```sql
CREATE TABLE ad_net.ads (
  id int not null auto_increment PRIMARY KEY, 
  text VARCHAR(255),
  image_id INT,
  adv_url VARCHAR(255)
);

CREATE TABLE ad_net.images (
  id int not null auto_increment PRIMARY KEY, 
  orig_name VARCHAR(255),
  unique_name VARCHAR(40)
);
```

some ruby that makes use of that:

```ruby
image = Image.create( :image_id => 2, :orig_name => "girl.jpg", :unique_name => "0000001.jpg" )
ad = Ad.create(:ad_id => 1, :text => "buy some stuff", :adv_url => "http://yahoo.com")

ad.image = image
ad.save

puts ad.inspect
```

# Business Domain

Now we have Advertisers who may have many ads.  So each ad, should
have an Advertiser id.

```sql
CREATE TABLE ad_net.ads (
  ...
  advertiser_id INT,
  ...
);
```

So ruby should be:

```ruby
class Ad < ActiveRecord::Base
  belongs_to :advertiser
end  
class Advertiser < ActiveRecord::Base
  has_many :ads
end
```

