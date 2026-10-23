'use strict';

// Mock upstream catalog service on its own port. It is "the network": the
// router must survive whatever it does, so a few ids misbehave on purpose.

const http = require('node:http');

const catalog = new Map([
  [1, { id: 1, name: 'Anvil', priceCents: 18900 }],
  [7, { id: 7, name: 'Bellows', priceCents: 4250 }],
  [42, { id: 42, name: 'Towel', priceCents: 1200 }],
  [123, { id: 123, name: 'Tongs', priceCents: 2799 }],
]);

const hostile = {
  13: (res) => res.writeHead(200, { 'content-type': 'application/json' }).end('not json'),
  14: (res) => res.writeHead(200, { 'content-type': 'application/json' }).end(JSON.stringify(catalog.get(42))),
  500: (res) => res.writeHead(500).end('upstream exploded'),
  666: () => {}, // never answers
  777: (res) => {
    res.writeHead(200, { 'content-length': '100' });
    res.write('{"id":7');
    setTimeout(() => res.socket.destroy(), 10);
  },
  888: (res) => res.writeHead(200).end(Buffer.alloc(1024 * 1024, 0x20)),
};

function startUpstream({ port = 3001, latencyMs = [5, 40] } = {}) {
  const sockets = new Set();
  const server = http.createServer((req, res) => {
    const match = /^\/items\/(\d+)$/.exec(req.url);
    const id = match ? Number(match[1]) : NaN;
    const delay = latencyMs[0] + Math.random() * (latencyMs[1] - latencyMs[0]);
    setTimeout(() => {
      if (hostile[id]) return hostile[id](res);
      const item = catalog.get(id);
      if (item === undefined) return res.writeHead(404).end();
      res.writeHead(200, { 'content-type': 'application/json' }).end(JSON.stringify(item));
    }, delay);
  });
  server.on('connection', (socket) => {
    sockets.add(socket);
    socket.on('close', () => sockets.delete(socket));
  });
  server.listen(port);
  return { server, sockets };
}

module.exports = { startUpstream };

if (require.main === module) {
  const { server } = startUpstream({ port: Number(process.env.PORT ?? 3001) });
  server.on('listening', () => console.log(`upstream on http://127.0.0.1:${server.address().port}`));
}
