if PLATFORM =~ /darwin/
  opts = "-dynamic -bundle -I/Library/Frameworks/R.framework/Resources/include"
  output = "ext.dylib"
else
  raise "dont know how to compile on platform #{PLATFORM}"
end

task :default do
  sh "g++ #{opts} ext.cc -o #{output}"
end
