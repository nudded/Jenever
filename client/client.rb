require "socket"
require 'digest/sha1'
class JeneverClient
  
  # specify host,port in a hash 
  # :host => "10.1.1.4" :port => 2625
  def initialize(host,port)
    @host, @port = host, port
    @passhash = '119941962e7c5978b916c5058cc0ae05c697b9d8'
  end
  
  def buy(user,number)
    if number < 0
      verify_user
    end
    send "#{user.upcase} #{number}" 
  end
  
  def status(user="")
    send "STATUS #{user}"
  end
  
  private
  
  def verify_user
    hash = Digest::SHA1::hexdigest(get_pass)
    raise "wrong password" if hash != @passhash
  end
  
  def get_pass
    print "input password: "
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
  ensure
    socket.close
  end
  
end
client = JeneverClient.new('localhost',2625)
p client.buy("nudded", 2)
p client.buy("nudded",-2)
p client.status
p client.status("jaspervdj")
