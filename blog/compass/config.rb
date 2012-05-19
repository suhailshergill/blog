require 'rubygems'
require 'bundler/setup'
# Require any additional compass plugins here.
require 'compass_twitter_bootstrap'

# # Set this to the root of your project when deployed:
# http_path = "/"
# css_dir = "stylesheets"
# sass_dir = "sass"
# images_dir = "images"
# javascripts_dir = "javascripts"

# You can select your preferred output style here (can be overridden via the command line):
# output_style = :expanded or :nested or :compact or :compressed

# To enable relative paths to assets via compass helper functions. Uncomment:
# relative_assets = true

# To disable debugging comments that display the original location of your selectors. Uncomment:
# line_comments = false


# If you prefer the indented syntax, you might want to regenerate this
# project again passing --syntax sass, or you can uncomment this:
# preferred_syntax = :sass
# and then run:
# sass-convert -R --from scss --to sass sass scss && rm -rf sass && mv scss sass

#Delineate the directory for our SASS/SCSS files (this directory)
sass_path = "compass" #  File.dirname(.);

staticDir = File.join(sass_path, "..", "static")

print "sasspath = #{sass_path} \n"
css_path = staticDir

# Delinate the images directory
images_dir = File.join(staticDir, "img")

print "images dir: #{images_dir} \n"

# Specify the output style/environment
#output_style = :expanded #:compressed
output_style = :compressed
environment = :production

$shouldTouch = true

on_stylesheet_saved do |filename|
  print "saved "
  print filename
  print "\n"
  if $shouldTouch
    $shouldTouch = false
    staticFile = "Settings.hs"
    `rm dist/build/Settings.o`
    `touch #{staticFile}`
    print "touched #{staticFile}\n"
  end
end
