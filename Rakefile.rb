desc "Build"
task :build do
    sh ('cabal build')
end

desc "Run with more resources"
task :run => [:build] do
    sh('./dist-newstyle/build/aarch64-osx/ghc-9.10.1/aoc2025-0.0.0.0/x/aoc2025/build/aoc2025/aoc2025 +RTS -M24G -RTS')
end