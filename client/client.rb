require "socket"

class JeneverClient
  
  # specify host,port in a hash 
  # :host => "10.1.1.4" :port => 2625
  def initialize(host,port)
    @host, @port = host, port
    @passhash = '119941962e7c5978b916c5058cc0ae05c697b9d8'
  end
  
  def drink(user,number)
    send "drink #{user.downcase} #{number}" 
  end
  
  def clean(user,number,pass=nil)
    pass = get_pass if pass == nil 
    send "clean #{user} #{number} #{pass}"
  end
  
  def change_password
    old = get_pass("old pass: ")
    new = get_pass("new pass: ")
    send "password #{old} #{new}"
  end
  
  def table
    send "table"
  end
  
  def status(user)
    send "status #{user}"
  end
  
  private
  
  def get_pass(text="input password: ")
    print text
    `stty -echo`
    pass = $stdin.gets.chomp
    `stty echo`
    puts
    pass
  end
  
  def send(command)
    socket = TCPSocket.new(@host,@port)
    socket.puts command
    return_value = ""
    while ! socket.eof?
      return_value << socket.gets
    end
    return_value
  rescue Errno::ECONNREFUSED
    puts "connection refused"
  ensure
    socket.close if socket
  end
  
end
temp = 2
threads = []
1.upto(400) do |i|
  threads << Thread.new(JeneverClient.new('10.1.1.74',2725)) do |client|
    2.times { client.drink "#{i}" ,temp }
  end
end
threads.each {|th| th.join}


