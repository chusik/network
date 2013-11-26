unit libssh;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CTypes;

const
  //* Hash Types */
  LIBSSH2_HOSTKEY_HASH_MD5   = 1;
  LIBSSH2_HOSTKEY_HASH_SHA1  = 2;

  //* Disconnect Codes (defined by SSH protocol) */
  SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT    = 1;
  SSH_DISCONNECT_PROTOCOL_ERROR                 = 2;
  SSH_DISCONNECT_KEY_EXCHANGE_FAILED            = 3;
  SSH_DISCONNECT_RESERVED                       = 4;
  SSH_DISCONNECT_MAC_ERROR                      = 5;
  SSH_DISCONNECT_COMPRESSION_ERROR              = 6;
  SSH_DISCONNECT_SERVICE_NOT_AVAILABLE          = 7;
  SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED = 8;
  SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE        = 9;
  SSH_DISCONNECT_CONNECTION_LOST                = 10;
  SSH_DISCONNECT_BY_APPLICATION                 = 11;
  SSH_DISCONNECT_TOO_MANY_CONNECTIONS           = 12;
  SSH_DISCONNECT_AUTH_CANCELLED_BY_USER         = 13;
  SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE = 14;
  SSH_DISCONNECT_ILLEGAL_USER_NAME              = 15;

type
  //* Session API */
  PLIBSSH2_SESSION = type Pointer;
  //* Malloc callbacks */
  LIBSSH2_ALLOC_FUNC = function(count: csize_t; abstract: Pointer): Pointer; cdecl;
  LIBSSH2_REALLOC_FUNC = function(ptr: Pointer; count: csize_t; abstract: Pointer): Pointer; cdecl;
  LIBSSH2_FREE_FUNC = procedure(ptr: Pointer; abstract: Pointer); cdecl;

var
  //* Session API */
  libssh2_session_init_ex: function(my_alloc: LIBSSH2_ALLOC_FUNC;
                                    my_free: LIBSSH2_FREE_FUNC;
                                    my_realloc: LIBSSH2_REALLOC_FUNC;
                                    abstract: Pointer): PLIBSSH2_SESSION; cdecl;
  libssh2_session_startup: function(session: PLIBSSH2_SESSION; sock: cint): cint; cdecl;
  libssh2_hostkey_hash: function(session: PLIBSSH2_SESSION; hash_type: cint): PAnsiChar; cdecl;
  libssh2_session_disconnect_ex: function(session: PLIBSSH2_SESSION;
                                          reason: cint;
                                          const description: PAnsiChar;
                                          const lang: PAnsiChar): cint; cdecl;
  libssh2_session_free: function(session: PLIBSSH2_SESSION): cint; cdecl;

  function libssh2_session_init: PLIBSSH2_SESSION; inline;
  function libssh2_session_disconnect(session: PLIBSSH2_SESSION; const description: PAnsiChar): cint; inline;

implementation

function libssh2_session_init: PLIBSSH2_SESSION;
begin
  Result:= libssh2_session_init_ex(nil, nil, nil, nil);
end;

function libssh2_session_disconnect(session: PLIBSSH2_SESSION; const description: PAnsiChar): cint;
begin
  Result:= libssh2_session_disconnect_ex(session, SSH_DISCONNECT_BY_APPLICATION,
                                         description, '');
end;

end.

