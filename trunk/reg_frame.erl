%% @author Staffan
%% @doc Register frame
%% @copyright 2011 Peer-talk
-module(reg_frame).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(ABOUT , ?wxID_ABOUT).
-define(SEND, 136).
-define(Preferences, 3).

%% @doc Init function for registration window. 
%% @spec chat_frame:start() -> no_return().
start() ->
    State = make_window(),
    loop(State),
    ok.

%% @doc 
%% @spec 
make_window() ->
	%% Create new wx-object, new window with panel and menubar...
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Pear Talk - Registration", [{size,{395, 360}}]),
    Panel = wxPanel:new(Frame, []),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer1 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Email"}]),
    Sizer2 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Username"}]),
    Sizer3 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Password"}]),
    Sizer4 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Repeat Password"}]),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
%% Buttons and Textfields
	B101  = wxButton:new(Panel, 101, [{label, "&Register"}]),
    B102  = wxButton:new(Panel, ?wxID_EXIT, [{label, "E&xit"}]),
    TextCtrl1 = wxTextCtrl:new(Panel, 201, [{value, "Email"},{style, ?wxDEFAULT}]),
    TextCtrl2 = wxTextCtrl:new(Panel, 202, [{value, "Username"},{style, ?wxDEFAULT}]), 
    TextCtrl3 = wxTextCtrl:new(Panel, 203, [{value, "password"},{style, ?wxDEFAULT bor ?wxTE_PASSWORD}]), 
    TextCtrl4 = wxTextCtrl:new(Panel, 204, [{value, "password"},{style, ?wxDEFAULT bor ?wxTE_PASSWORD}]), 
% Sizers:
	wxSizer:addSpacer(ButtonSizer, 220),
	wxSizer:add(ButtonSizer, B102,  []),
	wxSizer:addSpacer(ButtonSizer, 10), 
	wxSizer:add(ButtonSizer, B101,  []),
	wxSizer:addSpacer(ButtonSizer, 10), 
    wxSizer:add(Sizer1, TextCtrl1, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, TextCtrl2, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer3, TextCtrl3, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer4, TextCtrl4, [{flag, ?wxEXPAND}]),    
    wxSizer:add(MainSizer, Sizer1,  [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer2, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer3, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer4, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 20),
    wxSizer:add(MainSizer, ButtonSizer, []),
    wxPanel:setSizer(Panel, MainSizer),    
% Menubar:
	MenuBar = wxMenuBar:new(),
	File = wxMenu:new(),
	Help = wxMenu:new(),
	Preferences = wxMenu:new(),
% Some stuff in menu:
	wxMenu:append(File, 3, "Preferences\tCtrl-P"),
	wxMenu:append(File, ?SEND, "Send\tCtrl-S"),	
	wxMenuBar:append(MenuBar, File, "&File"),	
	wxMenuBar:append(MenuBar, Help, "&Help"),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame, "Thank you for signing up!"),
	wxMenuBar:enable(MenuBar, 3, false),
	wxMenuBar:enable(MenuBar,?SEND, false),
% create some listeners
    wxFrame:connect(Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),
    wxFrame:show(Frame),
% the return value
    {Server, Frame, TextCtrl1, TextCtrl2, TextCtrl3, TextCtrl4}.
 
loop(State) ->
    {Server, Frame, TextCtrl1, TextCtrl2, TextCtrl3, TextCtrl4} = State,
    receive
        #wx{event=#wxClose{}} ->
         	wxWindow:destroy(Frame), 
         	ok;  % exit the loop
 
    	#wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
         	wxWindow:destroy(Frame),
         	ok; 		
         	
    	#wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
	    case wxTextCtrl:getValue(TextCtrl3) == wxTextCtrl:getValue(TextCtrl4) of							
		false ->
		    Str = "The repeated password was not identical to the first.",
		    MD = wxMessageDialog:new(Frame,Str,
					     [{style, ?wxOK bor ?wxICON_INFORMATION},
					      {caption, "Notification!"}]),
		    wxDialog:showModal(MD),
		    wxDialog:destroy(MD),
		    loop(State);
		true ->
		    Mail = wxTextCtrl:getValue(TextCtrl1),  % kolla mot serverna om epost-adressen är upptagen
		    Password = wxTextCtrl:getValue(TextCtrl2),
		    ShowedName = vwTextCtrl:getValue(TextCtrl3),
		    chat ! {client, add_user, Mail, Password, ShowedName, self()},
		    receive
			{register, true} ->
			    ; % Låt servern lägga till användaren
			      % skicka en "reminder" till epost-adressen (lägg till ett valfritt meddelande till reminder.erl)
			{register, false} ->
			    Str = "The email address is already used.",
			    MD = wxMessageDialog:new(Frame,Str,
						     [{style, ?wxOK bor ?wxICON_INFORMATION},
						      {caption, "Notification!"}]),
			    wxDialog:showModal(MD),
			    wxDialog:destroy(MD),
			    loop(State)
		    end
	     end;											
	Msg ->
	    loop(State)
    end.
