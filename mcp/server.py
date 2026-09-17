#!/usr/bin/env python3
"""MCP 2025-11-25 stdio adapter for canonical core command answers."""
from __future__ import annotations
import argparse
import json
import os
from pathlib import Path
import re
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from transport import exchange, encode_request, FRAME_LIMIT

VERSION = '2025-11-25'
SCHEMA = {'type':'object','properties':{'argv':{'type':'array','items':{'type':'string'}}},'required':['argv'],'additionalProperties':False}

def tool_name(verb):
    suffix = verb if re.fullmatch(r'[A-Za-z0-9_.-]+', verb) and not verb.startswith('x_') else 'x_' + verb.encode().hex()
    name = 'theourgia_' + suffix
    if len(name) > 128:
        raise ValueError('Core tool name exceeds MCP limit')
    return name

class ProtocolError(Exception):
    def __init__(self, code, message):
        self.code, self.message = code, message

class Session:
    def __init__(self, store, actor, socket_path=None):
        self.store, self.actor, self.socket_path = store, actor, socket_path
        self.initialized = False
        self.ready = False
    def catalog(self):
        result = exchange(self.store, self.actor, 'human', b'(transport-verbs)\n', self.socket_path)
        if result.transport_error or result[0]:
            raise ProtocolError(-32603, 'Core catalog unavailable')
        try:
            verbs = [bytes.fromhex(line.decode('ascii')).decode('utf-8') for line in result[1].splitlines()]
            mapping = {tool_name(verb):verb for verb in verbs}
            if not mapping or len(mapping) != len(verbs):
                raise ValueError('Invalid catalog')
            return mapping
        except (ValueError, UnicodeError):
            raise ProtocolError(-32603, 'Core catalog unavailable')
    def handle(self, request):
        if not isinstance(request, dict) or request.get('jsonrpc') != '2.0' or not isinstance(request.get('method'), str):
            return {'jsonrpc':'2.0','id':None,'error':{'code':-32600,'message':'Invalid Request'}}
        identity = request.get('id')
        if 'id' in request and (isinstance(identity, bool) or not isinstance(identity, (str, int))):
            return {'jsonrpc':'2.0','id':None,'error':{'code':-32600,'message':'Invalid Request'}}
        method = request['method']
        params = request.get('params', {})
        notification = 'id' not in request
        if notification:
            if method == 'notifications/initialized' and self.initialized:
                self.ready = True
            return None
        try:
            if not isinstance(params, dict):
                raise ProtocolError(-32602, 'Invalid params')
            if method == 'initialize':
                if self.initialized or not isinstance(params.get('protocolVersion'), str) or not isinstance(params.get('capabilities'), dict) or not isinstance(params.get('clientInfo'), dict) or any(not isinstance(params['clientInfo'].get(k), str) for k in ('name','version')):
                    raise ProtocolError(-32602, 'Invalid initialization')
                self.initialized = True
                result = {'protocolVersion':VERSION,'capabilities':{'tools':{}},'serverInfo':{'name':'theourgia','version':'1'},
                          'instructions':'Tools return the unmodified core command answer as S-expression text. Core refusals are successful transport results. Eval is available only in the local CLI.'}
            elif method == 'ping':
                result = {}
            elif not self.ready:
                raise ProtocolError(-32600, 'Session is not initialized')
            elif method == 'tools/list':
                if params:
                    raise ProtocolError(-32602, 'Pagination is not available')
                result = {'tools':[{'name':name,'description':'Execute the core '+verb+' command and return its exact S-expression answer.','inputSchema':SCHEMA} for name,verb in self.catalog().items()]}
            elif method == 'tools/call':
                name = params.get('name')
                arguments = params.get('arguments')
                if not isinstance(name, str) or not isinstance(arguments, dict) or set(arguments) != {'argv'} or not isinstance(arguments['argv'], list) or any(not isinstance(arg,str) for arg in arguments['argv']):
                    raise ProtocolError(-32602, 'Invalid tool arguments envelope')
                catalog = self.catalog()
                if name not in catalog:
                    raise ProtocolError(-32602, 'Unknown tool')
                response = exchange(self.store, self.actor, 'wire', encode_request(catalog[name], arguments['argv']), self.socket_path)
                if response.transport_error:
                    raise ProtocolError(-32603, 'Core answer unavailable; execution may be unknown')
                # A core refusal is itself the requested command answer.
                result = {'content':[{'type':'text','text':response[1].decode('utf-8')}],'isError':False}
            else:
                raise ProtocolError(-32601, 'Method not found')
            return {'jsonrpc':'2.0','id':identity,'result':result}
        except ProtocolError as exc:
            return {'jsonrpc':'2.0','id':identity,'error':{'code':exc.code,'message':exc.message}}
        except (OSError, UnicodeError, ValueError):
            return {'jsonrpc':'2.0','id':identity,'error':{'code':-32603,'message':'Core transport unavailable'}}

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--store', required=True)
    parser.add_argument('--actor', default=os.environ.get('THEOURGIA_ACTOR') or os.environ.get('USER') or 'cli')
    parser.add_argument('--socket')
    args = parser.parse_args()
    session = Session(args.store, args.actor, args.socket)
    while True:
        line = sys.stdin.buffer.readline(FRAME_LIMIT+1)
        if not line:
            break
        if len(line) > FRAME_LIMIT or not line.endswith(b'\n'):
            answer = {'jsonrpc':'2.0','id':None,'error':{'code':-32600,'message':'Frame limit or incomplete frame'}}
            sys.stdout.write(json.dumps(answer,separators=(',',':'))+'\n');sys.stdout.flush()
            return 1
        try:
            request = json.loads(line, parse_constant=lambda value: (_ for _ in ()).throw(ValueError("Non-JSON number")))
            answer = session.handle(request)
        except (UnicodeError, ValueError, RecursionError):
            answer = {'jsonrpc':'2.0','id':None,'error':{'code':-32700,'message':'Parse error'}}
        if answer is not None:
            sys.stdout.write(json.dumps(answer,ensure_ascii=True,separators=(',',':'))+'\n')
            sys.stdout.flush()
    return 0

if __name__ == '__main__':
    sys.exit(main())
