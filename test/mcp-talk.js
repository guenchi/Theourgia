'use strict';

// Drives theourgia-mcp through initialize, the initialized notification and
// one tools/call of eval, answering on stdout with a summary line. Every
// line the shell writes to its stdout must be a JSON-RPC message; anything
// else is reported. usage: node mcp-talk.js <theourgia-mcp> <store>

const { spawn } = require('child_process');

const [mcp, store] = process.argv.slice(2);
const child = spawn(mcp, ['--store', store], { stdio: ['pipe', 'pipe', 'inherit'] });
let buffered = '';
const lines = [];
const waiting = new Map();

function send(message) {
  child.stdin.write(JSON.stringify(message) + '\n');
}
function answerTo(id) {
  return new Promise((resolve) => waiting.set(id, resolve));
}

child.stdout.on('data', (chunk) => {
  buffered += chunk.toString('utf8');
  let i;
  while ((i = buffered.indexOf('\n')) >= 0) {
    const line = buffered.slice(0, i);
    buffered = buffered.slice(i + 1);
    lines.push(line);
    let message = null;
    try {
      message = JSON.parse(line);
    } catch (e) {
      message = null;
    }
    if (message && waiting.has(message.id)) {
      waiting.get(message.id)(message);
      waiting.delete(message.id);
    }
  }
});

const deadline = setTimeout(() => {
  console.log(JSON.stringify({ timeout: true, lines }));
  child.kill('SIGTERM');
  process.exit(1);
}, 120000);

(async () => {
  send({ jsonrpc: '2.0', id: 1, method: 'initialize',
    params: { protocolVersion: '2025-06-18', capabilities: {}, clientInfo: { name: 'cells', version: '1' } } });
  const init = await answerTo(1);
  send({ jsonrpc: '2.0', method: 'notifications/initialized' });
  send({ jsonrpc: '2.0', id: 2, method: 'tools/call', params: { name: 'theourgia_eval', arguments: { argv: ['(+ 1 2)'] } } });
  const evaluated = await answerTo(2);
  child.stdin.end();
  // after 'close', not 'exit': the stream may still hold data when the
  // process has exited, and a line written last must be judged too
  child.on('close', (code) => {
    clearTimeout(deadline);
    if (buffered.length > 0) lines.push(buffered);
    // a message is a request or notification (method), or a response (id
    // with result or error); anything else on stdout is not the protocol
    const notJson = lines.filter((l) => {
      try {
        const m = JSON.parse(l);
        const request = typeof m.method === 'string';
        const response = 'id' in m && ('result' in m || 'error' in m);
        return !(m && m.jsonrpc === '2.0' && (request || response));
      } catch (e) {
        return true;
      }
    });
    const text = evaluated.result && evaluated.result.content && evaluated.result.content[0] &&
      evaluated.result.content[0].text;
    console.log(JSON.stringify({
      exit: code,
      initialized: !!init.result,
      evalText: text || null,
      evalIsError: evaluated.result ? evaluated.result.isError : null,
      stdoutLines: lines.length,
      notJson,
    }));
  });
})();
