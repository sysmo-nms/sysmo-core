# -*- mode: ruby -*-

# This file is part of Sysmo NMS.
#
# Sysmo NMS is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Sysmo NMS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Sysmo NMS.  If not, see <http://www.gnu.org/licenses/>.

require 'rubygems'
require 'rake'
require 'builder'
require 'pathname'

#
# set dirs vars
#
ROOT        = Dir.pwd
ERLANG_DIR  = ROOT
ERLANG_REL  = File.join(ROOT, "rel")
JAVA_DIR    = File.join(ROOT, "apps", "j_server", "priv", "jserver")
GO_DIR      = File.join(ROOT, "apps", "j_server", "priv", "pping")

PROD_RELEASE_DIR  = File.join(ROOT, "_build", "prod", "rel", "sysmo")
DEBUG_RELEASE_DIR = File.join(ROOT, "_build", "default", "rel", "sysmo")

#
# set wrappers vars
#
REBAR     = File.join(ERLANG_DIR, "rebar3")
GRADLE    = File.join(JAVA_DIR,   "gradlew")


#
# Shortcut stasks
#
task :default => :release
task :rel     => :debug_release


#
# tasks
#
desc "Build all"
task :build => [:java, :erl, :pping]


desc "Debug build. Send all levels of erlang error to sysmo.log"
task :debug_build => [:java, :debug_erl, :pping]


desc "Build GO pping"
task :pping do
  cd GO_DIR; sh "go build pping.go"
end


desc "Build Erlang project"
task :erl do
  cd ROOT; sh "#{REBAR} compile"
end


desc "Build debug Erlang project"
task :debug_erl do
  cd ROOT; sh "#{REBAR} compile"
end


desc "Build java"
task :java do
  cd JAVA_DIR;   sh "#{GRADLE} installDist"
end


#
# Clean tasks
#
desc "Clean"
task :clean do
  cd JAVA_DIR; sh "#{GRADLE} clean"
  cd ROOT;     sh "#{REBAR} clean"
  cd GO_DIR;   sh "go clean pping.go"
end


desc "Clean all"
task :clean_all => [:clean] do
  cd ROOT;
  FileUtils.rm_rf("_build")
end


#
# Test tasks
# 
desc "Test erlang and java apps"
task :test do
  cd ROOT;     sh "#{REBAR}  test"
  cd JAVA_DIR; sh "#{GRADLE} test"
end


desc "Check java apps"
task :check do
  cd JAVA_DIR; sh "#{GRADLE} check"
end



#
# Documentation tasks
#
desc "Generate documentation for java and erlang apps"
task :doc do
  cd ROOT;     sh "#{REBAR} doc"
  cd JAVA_DIR; sh "#{GRADLE} javadoc"
end



#
# Release Core tasks
#
desc "Generate Erlang release under the PRODUCTION profile"
task :release => [:build] do
  RELEASE_DIR = PROD_RELEASE_DIR;
  cd ROOT
  FileUtils.rm_rf("#{RELEASE_DIR}/java_apps")
  sh "#{REBAR} as prod release"
  install_pping_command()
  generate_all_checks()
  puts "Production release ready!"

end


desc "Generate Erlang release archive under the PRODUCTION profile"
task :release_archive => :release do
    cd ROOT; sh "#{REBAR} as prod tar"
end


desc "Generate Erlang release under the DEFAULT profile"
task :debug_release => [:debug_build] do
  RELEASE_DIR = DEBUG_RELEASE_DIR;
  cd ROOT
  FileUtils.rm_rf("#{RELEASE_DIR}/java_apps")
  sh "#{REBAR} release"
  install_pping_command()
  generate_all_checks()
  puts "Debug release ready!"
end


#
# Release Worker tasks
#
task :release_worker => [:java, :pping] do
  cd ROOT
  FileUtils.rm_rf("sysmo-worker")
  worker_dir = File.join(JAVA_DIR, "sysmo-worker/build/install/sysmo-worker")
  FileUtils.mv(worker_dir, "sysmo-worker")
  pping_exe = File.join(GO_DIR, "pping")
  FileUtils.mkdir("sysmo-worker/utils")
  FileUtils.cp(pping_exe, "sysmo-worker/utils/")
  ruby_dir = File.join(JAVA_DIR, "shared/nchecks/ruby")
  FileUtils.cp_r(ruby_dir, "sysmo-worker/ruby")
  FileUtils.mkdir("sysmo-worker/etc")
end


desc "Run the release in foreground"
task :run do
  cd ROOT
  sh "#{REBAR} console"
  sh "epmd -kill"
end


#
# pping special case
#
def install_pping_command()
  cd ROOT
  dst      = File.join(RELEASE_DIR, "utils")
  win_src  = File.join(GO_DIR, "pping.exe")
  unix_src = File.join(GO_DIR, "pping")
  if File.exist?(win_src)
    puts "Install #{win_src}"
    FileUtils.copy(win_src,dst)
  elsif File.exist?(unix_src)
    puts "Install #{unix_src}"
    FileUtils.copy(unix_src,dst)
  end
end


#
# generate AllChecks.xml
#
def generate_all_checks()
  cd ROOT
  puts "Building AllChecks.xml"

  # rm all checks informations if exists
  FileUtils.rm_f("#{RELEASE_DIR}/docroot/nchecks/AllChecks.xml")
  checks = Dir.glob("#{RELEASE_DIR}/docroot/nchecks/Check*.xml")

  # create new AllChecks.xml file
  file   = File.new("#{RELEASE_DIR}/docroot/nchecks/AllChecks.xml", "w:UTF-8")
  xml    = Builder::XmlMarkup.new(:target => file, :indent => 4)

  xml.instruct! :xml, :version=>"1.0", :encoding => "UTF-8"
  xml.tag!('NChecks', {"xmlns" => "http://schemas.sysmo.io/2015/NChecks"}) do
    xml.tag!('CheckAccessTable') do
      checks.each do |c|
        puts "Add CheckUrl Value=#{c}"
        xml.tag!('CheckUrl', {"Value" => Pathname.new(c).basename})
      end
    end
  end

  file.close()

end
