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

# set directories constants
SYSMO_ROOT   = Dir.pwd
JSERVER_ROOT = File.join(SYSMO_ROOT, "apps", "j_server", "priv", "jserver")
PPING_ROOT   = File.join(SYSMO_ROOT, "apps", "j_server", "priv", "pping")

# erlang releases location constants
PROD_RELEASE_DIR  = File.join(SYSMO_ROOT, "_build", "default", "rel", "sysmo")
DEBUG_RELEASE_DIR = File.join(SYSMO_ROOT, "_build", "debug",   "rel", "sysmo")

# set wrappers executable constants
REBAR  = File.join(SYSMO_ROOT, "rebar3")
GRADLE = File.join(JSERVER_ROOT, "gradlew")


# Shortcuts
desc "Build all project components."
task :build => ["sysmo:build", "jserver:build", "pping:build"]

desc "Clean all project components."
task :clean => ["sysmo:clean", "jserver:clean", "pping:clean"]

desc "Create a debug release"
task :rel => ["release:debug_build"]

desc "Run the debug release"
task :run => ["release:debug_run"]

desc "Create a production release"
task :release => ["release:build"]

desc "Create a production release"
task :release_worker => ["release:build_worker"]

desc "Clean environment"
task :clean_all => [:clean] do
    FileUtils.rm_rf("#{SYSMO_ROOT}/_build")
    FileUtils.rm_rf("#{SYSMO_ROOT}/sysmo-worker")
end

# Sysmo-Core release related tasks
namespace "release" do

    desc "Create a debug release"
    task :debug_build => ["sysmo:debug_build", "jserver:build", "pping:build"] do
        cd SYSMO_ROOT

        # remove old sysmo-jserver java application
        FileUtils.rm_rf("#{DEBUG_RELEASE_DIR}/java_apps")

        # generate release
        sh "#{REBAR} as debug release"

        # custom actions
        RELEASE_DIR = DEBUG_RELEASE_DIR
        install_pping_command()
        generate_all_checks()

        puts "Debug release ready!"
        # end
    end

    desc "Run the debug release"
    task :debug_run do
        sh "#{DEBUG_RELEASE_DIR}/bin/sysmo console"
        sh "epmd -kill"
    end

    desc "Create a production release"
    task :build => ["sysmo:build", "jserver:build", "pping:build"] do
        cd SYSMO_ROOT

        # remove old sysmo-jserver java application
        FileUtils.rm_rf("#{PROD_RELEASE_DIR}/java_apps")

        # generate release
        sh "#{REBAR} release"
        
        # custom actions
        RELEASE_DIR = PROD_RELEASE_DIR;
        install_pping_command()
        generate_all_checks()

        puts "Production release ready!"
        #end
    end

    desc "Generate a Sysmo-Worker"
    task :build_worker => ["jserver:build", "pping:build"] do
        cd SYSMO_ROOT

        # rm old release if exists
        FileUtils.rm_rf("sysmo-worker")

        # where is located sysmo-worker
        worker_dir = File.join(JSERVER_ROOT, "sysmo-worker/build/install/sysmo-worker")

        # move it here
        FileUtils.mv(worker_dir, "sysmo-worker")

        # put pping in (will fail on win32 wich is normal)
        pping_exe = File.join(PPING_ROOT, "pping")
        FileUtils.mkdir("sysmo-worker/utils")
        FileUtils.cp(pping_exe, "sysmo-worker/utils/")

        # put ruby scripts in
        ruby_dir = File.join(JSERVER_ROOT, "shared/nchecks/ruby")
        FileUtils.cp_r(ruby_dir, "sysmo-worker/ruby")
        FileUtils.mkdir("sysmo-worker/etc")

        # a worker does not require the xml checks definition
        
        puts "Worker release ready!"
        #end
    end

    desc "Generate Erlang release archive under the production profile"
    task :release_archive => :release do
        cd SYSMO_ROOT; sh "#{REBAR} tar"
        puts "Archive built in #{PROD_RELEASE_DIR}/"
    end

end


# Sysmo Erlang build and releases related tasks
namespace "sysmo" do

    desc "Build Sysmo-Core"
    task :build do
        cd SYSMO_ROOT
        sh "#{REBAR} compile"
    end

    desc "Build Sysmo-Core in DEBUG mode"
    task :debug_build do
        cd SYSMO_ROOT
        sh "#{REBAR} as debug compile"
    end

    desc "Clean Sysmo-Core"
    task :clean do
        cd SYSMO_ROOT
        sh "#{REBAR} clean"
        sh "#{REBAR} as debug clean"
    end

    desc "Clean all build directories"
    task :clean_all => [:clean] do
        cd SYSMO_ROOT
        FileUtils.rm_rf("_build")
        FileUtils.rm_f("rebar.lock")
    end

    desc "Test Sysmo-Core"
    task :test do
        # nothing to test yet
    end

    desc "Check Sysmo-Core"
    task :check do
        # nothing to check yet
    end

    desc "Generate documentation Sysmo-Core"
    task :doc do
        cd SYSMO_ROOT
        sh "#{REBAR} doc"
    end

end


# Sysmo-Jserver Java build and releases related tasks
namespace "jserver" do

    desc "Build Sysmo-Jserver"
    task :build do
        cd JSERVER_ROOT
        sh "#{GRADLE} installDist"
    end


    desc "Clean Sysmo-Jserver"
    task :clean do
        cd JSERVER_ROOT
        sh "#{GRADLE} clean"
    end

    desc "Test Sysmo-Jserver"
    task :test do
        cd JSERVER_ROOT
        sh "#{GRADLE} test"
    end

    desc "Check Sysmo-Jserver"
    task :check do
        cd JSERVER_ROOT
        sh "#{GRADLE} check"
    end

    desc "Generate documentation Sysmo-Jserver"
    task :doc do
        cd JSERVER_ROOT
        sh "#{GRADLE} javadoc"
    end

end


# Pping Golang build and releases related tasks
namespace "pping" do

    desc "Build Pping"
    task :build do
        cd PPING_ROOT
        sh "go build pping.go"
    end

    desc "Clean Pping"
    task :clean do
        cd PPING_ROOT;
        sh "go clean pping.go"
    end

    desc "Test Pping"
    task :test do
        # nothing to test yet
    end

    desc "Check Pping"
    task :check do
        # nothing to check yet
    end

    desc "Generate documentation Pping"
    task :doc do
        # no doc yet
    end

end

def install_pping_command()
  cd SYSMO_ROOT
  dst      = File.join(RELEASE_DIR, "utils")
  win_src  = File.join(PPING_ROOT, "pping.exe")
  unix_src = File.join(PPING_ROOT, "pping")
  if File.exist?(win_src)
    puts "Install #{win_src}"
    FileUtils.copy(win_src,dst)
  elsif File.exist?(unix_src)
    puts "Install #{unix_src}"
    FileUtils.copy(unix_src,dst)
  end
end

def generate_all_checks()
  cd SYSMO_ROOT
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
