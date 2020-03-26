Thermostat in Erlang

This is work-in-progress.  Published for illustration purposes.  Some
hardcoded configuration still needs to be removed.

Basic structure consists of these componets:

- thermostat Erlang process

- multiple thermometer nodes, currently implemented as linux boxes
  with USB thermometers attached

- actuator process, connects via ssh to a Linux box with a relay
  connected to a gpio

- web gui based on Cowboy and erl_tools web framework, connects to
  thermostat
  
