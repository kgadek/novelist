# vim: set syntax=ruby:

require 'colorize'
require 'artii'
require 'terminfo'
require 'pty'
require 'open3'
require 'time'

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Circuit breaker. Prevents the script from running multiple times, i.a. for
# a file saved multiple times.

# arbitrary small sleep time to ensure the "save all" is done
$lastbuildguard_epsilon = 1.0/10.0
$lastbuildguard_lastbuild = Time.now

def lastbuildguard(trigger, &block)
  begin
    sleep $lastbuildguard_epsilon

    if File.mtime(trigger) < $lastbuildguard_lastbuild
      puts "File #{trigger} modified before last build, so rebuilding is unnecessary".starsaround.green
    else
      $lastbuildguard_lastbuild = Time.now
      puts "\nBuilding at #{$lastbuildguard_lastbuild}\n"
      block.call()
      puts "\nBuild finished at #{Time.now}\n"
    end
  rescue SystemCallError => e
    puts "The run triggered by #{trigger} caused an error"
  end
end


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# String formatting, guerrilla-style.

class String
  def self.linefill_length
    TermInfo.screen_size[1]
  end

  def starfill
    x = self
    linefill_length = TermInfo.screen_size[1]
    if length % 2 != linefill_length % 2
      x = x + " "
    end
    x.ljust(linefill_length, " *")
  end

  def linefill
    linefill_length = TermInfo.screen_size[1]
    ljust(linefill_length, '-')
  end

  def self.starline
    linefill_length = TermInfo.screen_size[1]
    "*".ljust(linefill_length, " *")
  end

  def starsaround
    linefill_length = TermInfo.screen_size[1]
    stars = "*".ljust(linefill_length, " *")
    replace (stars + "\n" + self + "\n" + stars)
  end

  def starsallaround
    starfill.starsaround
  end

  def nocolourcodes
    gsub(/\e\[(\d+)(;\d+)*m/,'')
  end

  # Used for mulitline strings with variable line length
  def center_with_strlen(strlen)
    linefill_length = TermInfo.screen_size[1]
    len = linefill_length - strlen
    len = 0 if len < 0
    " " * (len / 2) + self
  end
end


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Run command. Measure execution time, print stdout+stderr and return code.

def command_interactive(cmd)
  puts String.starline.blue
  puts ("$ #{cmd}".blue + " 2>&1".yellow)

  stdout_empty = true
  
  start = Time.now
  begin
    PTY.spawn( cmd ) do |stdin, stdout, pid|
      begin
        puts "* * STDOUT & STDERR".starfill.blue
        stdin.each { |buf|
          stdout_empty = false
          buf.each_line {|line|
            # Hspec's QuickCheck outputs shell escape codes that break blue stars (even when said not to do so).
            # This hack circumvents the quirkology
            puts "  * ".blue + line.rstrip + "\r  * ".blue 
          }
        }
      rescue Errno::EIO
      end
      Process.wait(pid)
    end
  rescue PTY::ChildExited
    puts "  * Strange, the child process exited...".starsaround.red
  end
  finish = Time.now

  puts "  * ".blue + "STDOUT: none".black if stdout_empty

  diff = finish - start
  puts String.starline.blue
  if diff > 5 then puts ("  * ".blue + ("exec time: %.2f sec" % diff))
              else puts ("  * ".blue + ("exec time: %.2f sec" % diff).black)
  end

  if $?.exitstatus === 0  then puts ("  * ".blue + "exit code: #{$?.exitstatus}".black)
                          else puts "  * exit code: #{$?.exitstatus}".starsaround.red
  end

  if $?.exitstatus != 0
    unless $!.nil?
      raise SystemCallError.new("Execution of `#{cmd}` failed with error code: #{$!.exitstatus}")
    else
      raise SystemCallError.new("Execution of `#{cmd}` failed")
    end
  end
end

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Run command. Measure execution time, print stdout+stderr and return code.
# Passes input (stdin) if provided.

def command_withinput(cmd, inp=nil)
  puts "$ #{cmd}".starsaround.blue

  start = Time.now
  stdout, stderr, status = Open3.capture3(cmd, :stdin_data=>inp)
  finish = Time.now
  diff = finish - start

  if diff > 5 then puts ("  * ".blue + ("exec time: %.2f sec" % diff))
              else puts ("  * ".blue + ("exec time: %.2f sec" % diff).black)
  end

  if status.exitstatus === 0 then puts ("  * ".blue + "exit code: #{status.exitstatus}".black)
                             else puts ("  * ".blue + "exit code: #{status.exitstatus}".light_white)
  end

  unless stdout.empty?
    puts "* * STDOUT".starfill.green
    puts stdout.each_line.map {|l| "  * ".green + l}.join
  else
    puts "  * ".green + "STDOUT: none".black
  end

  unless stderr.empty?
    puts "* * STDERR".starsallaround.red
    puts stderr.each_line.map {|l| "  * ".red + l}.join
  end

  if status.exitstatus != 0
    raise SystemCallError.new("Execution of `#{cmd}` failed with error code: #{status.exitstatus}")
  end
end


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Pretty-printer of sections.

def section(name, *cmds, condition: true, noexception: false, &block)
  if condition
    begin
      grace = Artii::Base.new :font => 'univers'
      ascii_lines = grace.asciify(name).each_line
      ascii = ascii_lines.map { |line| line.center_with_strlen( ascii_lines.map(&:length).max ) }.join.cyan

      puts "".linefill.cyan
      puts ascii.cyan
      puts "".linefill.cyan

      cmds.map do |c| command_withinput(c) end

      block.call() unless block.nil?
    rescue SystemCallError => e
      unless noexception
        newe = $!.exception("Error in section '#{name}'\n#{$!}")
        raise newe
      end
    end
  end
end
