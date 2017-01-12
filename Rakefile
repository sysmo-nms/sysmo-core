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

system("git submodule update --init") 

#
SYSMO_CORE_VERSION_MAJOR = 2
SYSMO_CORE_VERSION_MINOR = 0
SYSMO_CORE_VERSION_PATCH = 1
SYSMO_CORE_VERSION = "#{SYSMO_CORE_VERSION_MAJOR}.#{SYSMO_CORE_VERSION_MINOR}.#{SYSMO_CORE_VERSION_PATCH}"

# set directories constants
SYSMO_ROOT   = Dir.pwd
JSERVER_ROOT = File.join(SYSMO_ROOT, "apps", "j_server", "priv", "sysmo-jserver")
PPING_ROOT   = File.join(SYSMO_ROOT, "apps", "j_server", "priv", "pping")
NCHECKS_REPO = File.join(SYSMO_ROOT, "apps", "j_server", "priv", "nchecks-base")

# erlang releases location constants
PROD_RELEASE_DIR  = File.join(SYSMO_ROOT, "_build", "default", "rel", "sysmo")
DEBUG_RELEASE_DIR = File.join(SYSMO_ROOT, "_build", "debug",   "rel", "sysmo")

# set wrappers executable constants
REBAR  = File.join(SYSMO_ROOT, "rebar3")
GRADLE = File.join(JSERVER_ROOT, "gradlew")


###############################################################################
## TASKS 
###############################################################################
task :default => [:debug_release]

desc "Clean."
task :clean => ["sysmo:clean", "jserver:clean", "pping:clean"]


desc "Shortcut for debug_release"
task :rel => [:debug_release]

desc "Create a debug release."
task :debug_release => ["sysmo:debug_build", "jserver:build", "pping:build"] do
    cd SYSMO_ROOT

    # remove old sysmo-jserver java application wich may be present
    FileUtils.rm_rf("#{DEBUG_RELEASE_DIR}/java_apps")

    # generate release
    sh "#{REBAR} as debug release"

    # extern install
    install_pping(DEBUG_RELEASE_DIR)
    install_nchecks(DEBUG_RELEASE_DIR, true)

    puts "Debug release ready!"
    puts ""
    puts "Execute 'rake run' to start the service."
    # end
end


desc "Run the debug release."
task :run do
    sh "#{DEBUG_RELEASE_DIR}/bin/sysmo console"
    sh "epmd -kill"
end


desc "Create a production release."
task :release => ["jserver:build", "pping:build"] do
    cd SYSMO_ROOT

    # remove old release
    clean_all()

    # generate release
    sh "#{REBAR} release"

    # extern install
    install_pping(PROD_RELEASE_DIR)
    install_nchecks(PROD_RELEASE_DIR, false)

    puts "Production release ready!"
    #end
end


desc "Generate a platform specific package or installer."
task :pack => :release do

    if (/cygwin|mswin|mingw|bccwin|wince|emx|win/ =~ RUBY_PLATFORM)
        pack_win32()
    elsif (/darwin/ =~ RUBY_PLATFORM)
        pack_macos()
    elsif (/linux/ =~ RUBY_PLATFORM)
        pack_linux()
    else
        pack_other()
    end

end


desc "Clean environment."
task :clean_all => [:clean] do 
    clean_all()
end


desc "Test"
task :test => ["sysmo:test", "jserver:test", "pping:test"]


desc "Check"
task :check => ["sysmo:check", "jserver:check", "pping:check"]


desc "Generate documentation"
task :doc => ["sysmo:doc", "jserver:doc", "pping:doc"]


###############################################################################
## NAMESPACES
###############################################################################

# Sysmo Erlang build and releases related tasks
namespace "sysmo" do

    file "rebar.config" => ["rebar.config.in", "Rakefile"] do
        configure_file("rebar.config.in", "rebar.config")
    end

    file "apps/sysmo/src/sysmo.app.src" => 
        ["apps/sysmo/src/sysmo.app.src.in", "Rakefile"] do
        configure_file(
            "apps/sysmo/src/sysmo.app.src.in", 
            "apps/sysmo/src/sysmo.app.src")
    end

    task :configure_files => ["rebar.config", "apps/sysmo/src/sysmo.app.src"]

    # "Build Sysmo-Core"
    task :build => [:configure_files] do
        cd SYSMO_ROOT
        sh "#{REBAR} compile"
    end

    # "Build Sysmo-Core in DEBUG mode"
    task :debug_build => [:configure_files] do
        cd SYSMO_ROOT
        sh "#{REBAR} as debug compile"
    end

    # "Clean Sysmo-Core"
    task :clean => [:configure_files] do
        cd SYSMO_ROOT
        sh "#{REBAR} clean"
        sh "#{REBAR} as debug clean"
    end

    # "Test Sysmo-Core"
    task :test => [:configure_files] do
        cd SYSMO_ROOT
        sh "#{REBAR} eunit"
    end

    # "Check Sysmo-Core"
    task :check do
        # nothing to check yet
    end

    # "Generate documentation Sysmo-Core"
    task :doc => [:configure_files] do
        cd SYSMO_ROOT
        sh "#{REBAR} edoc"
    end

end


# Sysmo-Jserver Java build and releases related tasks
namespace "jserver" do

    # "Build Sysmo-Jserver"
    task :build do
        cd JSERVER_ROOT
        sh "#{GRADLE} installDist"
    end


    # "Clean Sysmo-Jserver"
    task :clean do
        cd JSERVER_ROOT
        sh "#{GRADLE} clean"
    end

    # "Test Sysmo-Jserver"
    task :test do
        cd JSERVER_ROOT
        sh "#{GRADLE} test"
    end

    # "Check Sysmo-Jserver"
    task :check do
        cd JSERVER_ROOT
        sh "#{GRADLE} check"
    end

    # "Generate documentation Sysmo-Jserver"
    task :doc do
        cd JSERVER_ROOT
        sh "#{GRADLE} javadoc"
    end

end


# Pping Golang build and releases related tasks
namespace "pping" do

    # "Build Pping"
    task :build do
        cd PPING_ROOT
        sh "go build pping.go"
    end

    # "Clean Pping"
    task :clean do
        cd PPING_ROOT;
        sh "go clean pping.go"
    end

    # "Test Pping"
    task :test do
        # nothing to test yet
    end

    # "Check Pping"
    task :check do
        # nothing to check yet
    end

    # "Generate documentation Pping"
    task :doc do
        # no doc yet
    end
end



###############################################################################
## FUNCTIONS
###############################################################################

#
# generate a wix package bundle
#
def pack_win32()
    puts ":: Generate #{RUBY_PLATFORM} package"
end

#
# generate an osx app installer
#
def pack_macos()
    puts ":: Generate #{RUBY_PLATFORM} package"
end

#
# detect linux flavor and generate a RPM or DEB
#
def pack_linux()
    puts ":: Generate #{RUBY_PLATFORM} package"
    sh "#{REBAR} tar"
    puts "Archive built in #{PROD_RELEASE_DIR}/"
end

#
# generate an archive for other unixes
#
def pack_other()
    puts "Generate #{RUBY_PLATFORM} package"
    sh "#{REBAR} tar"
    puts "Archive built in #{PROD_RELEASE_DIR}/"
end

#
# Remove build directories
#
def clean_all()
    puts ":: Clean all"
    cd SYSMO_ROOT
    FileUtils.rm_rf("_build")
    FileUtils.rm_rf("sysmo-worker")
end

#
# Install pping command in the specified release directory
# 
def install_pping(release_dir)
    puts ":: Install pping"
    cd SYSMO_ROOT

    dst      = File.join(release_dir, "utils")
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

#
# Install nchecks definitions and scripts in the release directory
# 
def install_nchecks(release_dir, include_dummy)
    puts ":: Building NChecksRepository.xml"
    cd SYSMO_ROOT

    # cleanup
    FileUtils.rm_rf("#{release_dir}/docroot/nchecks")
    FileUtils.rm_rf("#{release_dir}/etc/nchecks")
    FileUtils.rm_rf("#{release_dir}/ruby")

    # put all xml files where required
    FileUtils.mkdir("#{release_dir}/docroot/nchecks")
    FileUtils.mkdir("#{release_dir}/etc/nchecks")

    ppXml = "#{PPING_ROOT}/io.sysmo.nchecks.CheckICMP.xml"
    FileUtils.cp(ppXml, "#{release_dir}/docroot/nchecks/")
    FileUtils.cp(ppXml, "#{release_dir}/etc/nchecks/")

    rbXml = Dir.glob("#{NCHECKS_REPO}/io.sysmo.nchecks.*.xml")
    rbXml.each do |x|
        FileUtils.cp(x, "#{release_dir}/docroot/nchecks/")
        FileUtils.cp(x, "#{release_dir}/etc/nchecks/")
    end

    if include_dummy == true
        dummyXml = Dir.glob("#{NCHECKS_REPO}/dummy/io.sysmo.nchecks.*.xml")
        dummyXml.each do |x|
            FileUtils.cp(x, "#{release_dir}/docroot/nchecks/")
            FileUtils.cp(x, "#{release_dir}/etc/nchecks/")
        end
    end

    # put all ruby scripts
    FileUtils.mkdir("#{release_dir}/ruby")
    rbScripts = Dir.glob("#{NCHECKS_REPO}/io.sysmo.nchecks.*.rb")
    rbScripts.each do |x|
        FileUtils.cp(x, "#{release_dir}/ruby/")
    end

    # create new NChecksRepository.xml file
    file   = File.new("#{release_dir}/docroot/nchecks/NChecksRepository.xml", "w:UTF-8")
    checks = Dir.glob("#{release_dir}/docroot/nchecks/io.sysmo.nchecks.*.xml")

    xml = Builder::XmlMarkup.new(:target => file, :indent => 4)
    xml.instruct! :xml, :version=>"1.0", :encoding => "UTF-8"
    xml.tag!('NChecks', {"xmlns" => "http://schemas.sysmo.io/2015/NChecks"}) do
        xml.tag!('Repository') do
            checks.each do |c|
                checkId = "#{Pathname.new(c).basename()}".chomp('.xml')
                puts "Add Check Id=#{checkId}"
                xml.tag!('Check', {"Id" => checkId})
            end
        end
    end

    file.close()
end

#
# Configure file
# 
def configure_file(file_name_in, file_name_out)
    text = File.read(file_name_in)

    Module.constants.each do |x|
        search_text = "@#{x}@"
        replace_with = Module.const_get(x)
        other = text.gsub(search_text.to_s, replace_with.to_s)
        text = other
    end

    File.open(file_name_out, "w") { |file| file.puts text }
end
