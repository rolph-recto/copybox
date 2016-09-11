## Copybox

A web interface for delivering local files. This is useful for accessing
`rsync`ed directories stored in a VPS -- your very own knockoff Dropbox!

The server is written using the magical `servant` library, while the
client is written in Purescript using the `thermite` library.  

### Configuration
Currently the backend is a Servant application running atop a Warp server,
as is the default. The server delivers both the backend (which sends JSON data)
and the frontend (which receives the JSON data and displays it). You can
delete the 'web' endpoint in the Copybox API type if you want to host the
client somewhere else.
