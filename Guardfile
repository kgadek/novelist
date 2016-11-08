# vim: set syntax=ruby:

################################################################################
# ABOUT
################################################################################
#
# Guard[1] configuration.
# 
#     Guard is a command line tool to easily handle events on
#     file system modifications.
#
# First, you need a Ruby. I'm assuming rbenv[2] is already installed. Then, the
# process goes as follows::
#
#     rbenv install 2.3.1
#     rbenv local 2.3.1
#     eval "$(rbenv init -)"    # To make freshly installed Ruby visible in
#                               # current terminal.
#     gem install guard-shell colorize artii colorize ruby-terminfo
#
# Links:
# [1]: https://github.com/guard/guard
# [2]: https://github.com/rbenv/rbenv
#
#
################################################################################
# CONFIGURATION
################################################################################

$tests_compiled = false
$tests_compiled_coverage = false

################################################################################
# THE ACTUAL DSL
################################################################################

require './guard_tools.rb'

group :haskell, halt_on_fail: true do

  guard :shell do

    watch(%r{^(src|test|bench)/.*\.hs$}) do |m|
      lastbuildguard(m[0]) do
        if $tests_compiled
          section "tests (c)" do
            command_interactive "stack test"
            coverage("novelist-test")
          end
        else
          section "tests (i)" do
            command_interactive "stack exec -- runhaskell -isrc -itest test/Spec.hs"
          end
        end
      end
    end

  end

  guard :shell do

    watch(%r{^(.*\.cabal|stack.yaml)$}) do |m|
      lastbuildguard(m[0]) do
        section "project" do
          command_interactive "stack build --test --no-run-tests --bench --no-run-benchmarks"
        end
      end
    end

    watch(%r{^app/.*\.hs$}) do |m|
      lastbuildguard(m[0]) do
        section "app" do
          command_interactive "stack install"
        end
      end
    end

    watch(%r{^bench/.*\.hs$}) do |m|
      section "bench" do
        command_interactive "stack build --bench"
      end
    end

  end
end


# In case of strange errors, this hammer seems to work well:
#
#     rm -rf .stack-work .hpc *.tix hpc_report
#

def coverage(project_name)
  section "coverage", "rm -rf hpc_report", :condition => $tests_compiled_coverage, :noexception => true do
    hpc_excluded_modules = ((Dir.glob("test/**/*Spec.hs")          # skip all test-spec files
                                .map { |k| k.gsub(/^test\//, "")     # ...converting path to namespace for HPC
                                            .gsub(/\.hs$/,"")
                                            .gsub("/",".")
                                     }
                            ) << "Main"                            # and skip "Main", the entrypoint for tests
                           ).map{|k| "--exclude=#{k}" }.join(" ")
    command_interactive "hpc report #{project_name}.tix #{hpc_excluded_modules}"
    command_interactive "hpc markup #{project_name}.tix --destdir=hpc_report #{hpc_excluded_modules}"
  end
end
