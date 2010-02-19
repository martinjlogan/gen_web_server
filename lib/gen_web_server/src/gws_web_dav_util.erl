%%%-------------------------------------------------------------------
%%% @author Martin Logan <martinjlogan@Macintosh.local>
%%% @copyright (C) 2010, Martin Logan
%%% @doc A helper module for doing WEBDAV (RFC 4918) interaction. This
%%%      is curretly only used for the Portius release and is very much
%%%      incomplete.
%%%    *** READ - the whole gws_web_dav_util module is a HACK use at
%%%               your own risk
%%% @end
%%% Created : 11 Feb 2010 by Martin Logan <martinjlogan@Macintosh.local>
%%%-------------------------------------------------------------------
-module(gws_web_dav_util).

%% API
-export([
	 mkcol/2,
	 delete/2,
	 propfind/4
	]).

-include_lib("kernel/include/file.hrl").
-include("eunit.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc MKCOL method helper. Correct reponse after this is 201 created.
%% @spec (DocumentRoot, AbsPath) -> ok
%% @end
%%--------------------------------------------------------------------
mkcol(DocumentRoot, AbsPath) ->
    mkdir_p(filename:join(DocumentRoot, string:strip(AbsPath, left, $\/))).

%%--------------------------------------------------------------------
%% @doc DELETE method helper. If a failure occurs the response should
%% be either 207 or a 4xx if the response failed entirely. If a lock
%% got in the way of the delete then a 423 (locked) should be returned
%% @spec (DocumentRoot, AbsPath) -> ok
%% @end
%%--------------------------------------------------------------------
delete(DocumentRoot, AbsPath) ->
    delete_dir(filename:join(DocumentRoot, string:strip(AbsPath, left, $\/))).
    
%%--------------------------------------------------------------------
%% @doc PROPFIND method helper. 
%% @spec (DocumentRoot, AbsPath, Host, Depth) -> ok
%% @end
%%--------------------------------------------------------------------
propfind(DocumentRoot, AbsPath, Host, Depth) ->
    Path = filename:join(DocumentRoot, string:strip(AbsPath, left, $\/)),
    io:format("path ~p~n", [Path]),
    WildCard = lists:foldl(fun(_Num, Path_) -> Path_ ++ "/*" end, Path, lists:seq(1, Depth)),
    FilePaths = filelib:wildcard(WildCard),
    % returns {TruncatedPath, ReadFileInfo}
    create_multistatus_response(DocumentRoot, Host,[string:strip(Path, right, $\/)|FilePaths]).

create_multistatus_response(DocumentRoot, Host, FilePaths) ->
    case create_responses(DocumentRoot, Host, FilePaths) of
	[] ->
	    error;
	Responses ->
	    lists:flatten(["<?xml version=\"1.0\" encoding=\"utf-8\"?>",
			   "<D:multistatus xmlns:D=\"DAV:\" xmlns:ns0=\"urn:uuid:c2f41010-65b3-11d1-a29f-00aa00c14882/\">",
			   Responses,
			   "</D:multistatus>"])
    end.

create_responses(DocumentRoot, Host, [FilePath|T]) -> 
    TruncatedPath      = string:substr(FilePath, length(DocumentRoot) + 1, length(FilePath)), 
    case catch file:read_file_info(FilePath) of
	{ok, ReadFileInfo} ->
	    URL                = "http://" ++ Host ++ "/" ++ TruncatedPath,
	    [
	     "<D:response>",
	     "<D:href>", URL, "</D:href>",
	     "<D:propstat>",
	     "<D:prop>",
	     "<D:creationdate ns0:dt=\"dateTime.tz\">", date_string(element(7,ReadFileInfo)), "</D:creationdate><D:getcontentlanguage>en</D:getcontentlanguage><D:getcontentlength>512</D:getcontentlength><D:getcontenttype>httpd/unix-directory</D:getcontenttype><D:getlastmodified ns0:dt=\"dateTime.rfc1123\">", httpd_util:rfc1123_date(element(6,ReadFileInfo)), "</D:getlastmodified><D:resourcetype><D:collection/></D:resourcetype>",
	     "</D:prop>",
	     "<D:status>HTTP/1.1 200 OK</D:status>",
	     "</D:propstat>",
	     "</D:response>"
	    ] ++ create_responses(DocumentRoot, Host, T);
	_Error ->
	    create_responses(DocumentRoot, Host, T)
    end;
create_responses(_DocumentRoot, _Host, []) -> 
    [].




    

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%-------------------------------------------------------------------
%% @private
%% @doc
%% Makes a directory including parent dirs if they are missing.
%% @spec mkdir_p(Path) -> ok | exit()
%% @end
%%-------------------------------------------------------------------
mkdir_p(Path) ->
    case erlang:system_info(system_architecture) of
	"win32" ->
	    filelib:ensure_dir(lists:flatten([filename:absname(Path), "\\"]));
	_SysArch ->
	    filelib:ensure_dir(lists:flatten([filename:absname(Path), "/"]))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc delete a non empty directory.
%% @spec delete_dir(Path) -> ok 
%% @end
%%--------------------------------------------------------------------
delete_dir(Path) ->
    case filelib:is_dir(Path) of
	false ->
	    case filelib:is_file(Path) of
		false -> 
		    case file:read_link_info(Path) of
			{ok, LinkInfo} ->
			    %% XXX Exploiting the structure of a record, tisk, tisk, should probably include the proper .hrl file.
			    symlink = element(3, LinkInfo),
			    ok = file:delete(Path);
			_ ->
			    error_logger:info_msg("delete_dir/1 file does not exist ~p~n", [Path]), ok
		    end;
		true -> 
		    ok = file:delete(Path)
	    end;
	true ->
	    lists:foreach(fun(ChildPath) -> delete_dir(ChildPath) end, filelib:wildcard(Path ++ "/*")),
	    ok = file:del_dir(Path)
    end.

date_string({{Y,Mo,D},{H,Min,S}}) ->
    lists:concat([Y, "-", Mo, "-", D, "T", H, ":", Min, ":", S, "Z"]).
