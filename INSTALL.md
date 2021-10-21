## MS1 Instructions:
1) Clone the repository, and navigate to the root directory of the repository
2) Make sure the packages Core and Async are installed. To do this run "opam install core" and "opam install async"
2) To run the server, run "dune build ./server/server.exe" and then run "dune exec ./server/server.exe"
3) To run the client, run "dune build ./client/client.exe" and then run "dune exec ./client/client.exe \[ip\] 8765" where \[ip\] is the IP address of the machine that the server is running on.
  - To get the IP address of the server (for Mac), go to a new terminal tab on the machine running the server. Type "ifconfig" and hit enter. The "inet" address under the "en0" interface is the public IP address of the machine.
4) Any messages sent will be stored by the server and displayed to the next user that connects. When sending a message, format it as "\[your name\]: \[your message\]" so that when the messages are displayed it is clear who sent it.