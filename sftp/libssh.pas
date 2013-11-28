unit libssh;

{$mode objfpc}{$H+}
{$packrecords c}

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

  //* Flags for open_ex() */
  _LIBSSH2_SFTP_OPENFILE                        = 0;
  _LIBSSH2_SFTP_OPENDIR                         = 1;

type
  //* Session API */
  PLIBSSH2_SESSION = type Pointer;
  //* SFTP API */
  PLIBSSH2_SFTP = type Pointer;
  PLIBSSH2_SFTP_HANDLE = type Pointer;
  PLIBSSH2_SFTP_ATTRIBUTES = ^LIBSSH2_SFTP_ATTRIBUTES;
  LIBSSH2_SFTP_ATTRIBUTES = record
    flags: culong;
    filesize: cuint64;
    uid, gid: culong;
    permissions: culong;
    atime, mtime: culong;
  end;
  //* Malloc callbacks */
  LIBSSH2_ALLOC_FUNC = function(count: csize_t; abstract: Pointer): Pointer; cdecl;
  LIBSSH2_REALLOC_FUNC = function(ptr: Pointer; count: csize_t; abstract: Pointer): Pointer; cdecl;
  LIBSSH2_FREE_FUNC = procedure(ptr: Pointer; abstract: Pointer); cdecl;
  //* Callbacks for special SSH packets */
  LIBSSH2_PASSWD_CHANGEREQ_FUNC = procedure(session: PLIBSSH2_SESSION; var newpw: PAnsiChar;
                                            var newpw_len: cint; abstract: Pointer); cdecl;

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
  libssh2_session_set_blocking: procedure(session: PLIBSSH2_SESSION; blocking: cint); cdecl;

  //* Userauth API */
  libssh2_userauth_password_ex: function(session: PLIBSSH2_SESSION;
                                         const username: PAnsiChar;
                                         username_len: cuint;
                                         const password: PAnsiChar;
                                         password_len: cuint;
                                         passwd_change_cb: LIBSSH2_PASSWD_CHANGEREQ_FUNC): cint; cdecl;
  //* SFTP API */
  libssh2_sftp_init: function(session: PLIBSSH2_SESSION): PLIBSSH2_SFTP; cdecl;
  libssh2_sftp_shutdown: function(sftp: PLIBSSH2_SFTP): cint; cdecl;
  //* File / Directory Ops */
  libssh2_sftp_open_ex: function(sftp: PLIBSSH2_SFTP;
                                 const filename: PAnsiChar;
                                 filename_len: cint; flags: culong;
                                 mode: clong; open_type: cint): PLIBSSH2_SFTP_HANDLE; cdecl;
   libssh2_sftp_readdir_ex: function(handle: PLIBSSH2_SFTP_HANDLE;
                                     buffer: PAnsiChar; buffer_maxlen: csize_t;
                                     longentry: PAnsiChar; longentry_maxlen: csize_t;
                                     attrs: PLIBSSH2_SFTP_ATTRIBUTES): cint; cdecl;
  libssh2_sftp_close_handle: function(handle: PLIBSSH2_SFTP_HANDLE): cint; cdecl;

  function libssh2_session_init: PLIBSSH2_SESSION; inline;
  function libssh2_session_disconnect(session: PLIBSSH2_SESSION; const description: PAnsiChar): cint; inline;
  function libssh2_userauth_password(session: PLIBSSH2_SESSION; const username: PAnsiChar; const password: PAnsiChar): cint; inline;
  function libssh2_sftp_open(sftp: PLIBSSH2_SFTP; const filename: PAnsiChar; flags: culong; mode: clong): PLIBSSH2_SFTP_HANDLE; inline;
  function libssh2_sftp_opendir(sftp: PLIBSSH2_SFTP; const path: PAnsiChar): PLIBSSH2_SFTP_HANDLE; inline;
  function libssh2_sftp_close(handle: PLIBSSH2_SFTP_HANDLE): cint; inline;
  function libssh2_sftp_closedir(handle: PLIBSSH2_SFTP_HANDLE): cint; inline;

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

function libssh2_userauth_password(session: PLIBSSH2_SESSION;
  const username: PAnsiChar; const password: PAnsiChar): cint;
begin
  Result:= libssh2_userauth_password_ex(session, username, strlen(username),
                                        password, strlen(password), nil);
end;

function libssh2_sftp_open(sftp: PLIBSSH2_SFTP; const filename: PAnsiChar;
  flags: culong; mode: clong): PLIBSSH2_SFTP_HANDLE;
begin
  Result:= libssh2_sftp_open_ex(sftp, filename, strlen(filename), flags, mode, _LIBSSH2_SFTP_OPENFILE);
end;

function libssh2_sftp_opendir(sftp: PLIBSSH2_SFTP; const path: PAnsiChar): PLIBSSH2_SFTP_HANDLE;
begin
  Result:= libssh2_sftp_open_ex(sftp, path, strlen(path), 0, 0, _LIBSSH2_SFTP_OPENDIR);
end;

function libssh2_sftp_close(handle: PLIBSSH2_SFTP_HANDLE): cint;
begin
  Result:= libssh2_sftp_close_handle(handle);
end;

function libssh2_sftp_closedir(handle: PLIBSSH2_SFTP_HANDLE): cint;
begin
  Result:= libssh2_sftp_close_handle(handle);
end;

end.

