-module(dialog).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

make_window(Obj) ->
    {{_, Sender_showed_name}, FileName, _} = Obj,
    Line = io_lib:format("accept file ~p from ~p (y/n)? ", [FileName, Sender_showed_name]),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "pear", [{size,{300,300}}]),
    MD = wxMessageDialog:new(Frame, Line, [{style, ?wxYES_NO bor ?wxSTAY_ON_TOP bor ?wxICON_INFORMATION},{caption, "File Transfer"}]),
    case wxDialog: showModal(MD) of
	?wxID_YES ->
	    chat!{file_accept, Obj};
	?wxID_NO ->
	    chat!{file_refuse, Obj}
    end,
    wxDialog:destroy(MD).

acc_friend(Obj) ->
    {friendaccept, Sender_username, Sender_showed_name} = Obj,
    Line = io_lib:format("accept friend request from ~p as ~p (y/n)? ", [Sender_username, Sender_showed_name]),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "pear", [{size,{300,300}}]),
    MD = wxMessageDialog:new(Frame, Line, [{style, ?wxYES_NO bor ?wxICON_INFORMATION bor ?wxCANCEL bor ?wxSTAY_ON_TOP},{caption, "Friend Request"}]),
    case wxDialog: showModal(MD) of
	?wxID_YES ->
	    chat!{confirm, Sender_username};
	?wxID_NO ->
	    chat!{refuse, Sender_username};
	_ ->
		[]
    end,
    wxDialog:destroy(MD).
