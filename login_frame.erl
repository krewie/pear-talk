%% @author Staffan
%% @doc First draft of the chat window 
%% @copyright 2011 Peer-talk
-module(login_frame).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(ABOUT , ?wxID_ABOUT).
-define(SEND, 136).
-define(Preferences, 3).

%% @doc Init function for chat window. 
%% @spec chat_frame:start() -> no_return().
start(User) ->
    State = make_window(User),
    loop(State),
    ok.

connect_to_server() -> 0.

%% @doc 
%% @spec 
make_window(User) ->
	%% Create new wx-object, new window with panel and menubar...
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Pear Talk", [{size,{365, 320}}]),
    Panel = wxPanel:new(Frame, []),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Email"}]),
    Sizer2 = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Password"}]),
    ButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
    %%
    B101  = wxButton:new(Panel, 101, [{label, "&Go!"}]),
    B102  = wxButton:new(Panel, ?wxID_EXIT, [{label, "E&xit"}]),
    B103  = wxButton:new(Panel, 103, [{label, "&Register"}]),		      
    B104  = wxButton:new(Panel, 104, [{label, "Forgot Password?"}]),
    TextCtrl = wxTextCtrl:new(Panel, 201, [{value, User},{style, ?wxDEFAULT}]),
    TextCtrl2 = wxTextCtrl:new(Panel, 202, [{value, "password"},{style, ?wxDEFAULT bor ?wxTE_PASSWORD}]), 
    %Icon:
    BitmapSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),
    Image = wxImage:new("icon2.jpg", []),
    Bitmap = wxBitmap:new(wxImage:scale(Image, round(wxImage:getWidth(Image)*0.34),
					round(wxImage:getHeight(Image)*0.25),
					[{quality, ?wxIMAGE_QUALITY_HIGH}])),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap),
    % Sizers:
    wxSizer:addSpacer(MainSizer, 105), 
    wxSizer:addSpacer(ButtonSizer, 10),
    wxSizer:add(ButtonSizer, B103,  []),
    wxSizer:addSpacer(ButtonSizer, 10), 
    wxSizer:add(ButtonSizer, B104,  []),
    wxSizer:addSpacer(ButtonSizer, 10), 
    wxSizer:add(ButtonSizer, B102,  []),
    wxSizer:addSpacer(ButtonSizer, 10),
    wxSizer:add(ButtonSizer, B101,  []),
    wxSizer:add(Sizer, TextCtrl, [{flag, ?wxEXPAND}]),
    wxSizer:add(Sizer2, TextCtrl2, [{flag, ?wxEXPAND}]),
    wxSizer:add(MainSizer, Sizer,  [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 10),
    wxSizer:add(MainSizer, Sizer2, [{flag, ?wxEXPAND}]),
    wxSizer:addSpacer(MainSizer, 20),
    wxSizer:add(MainSizer, ButtonSizer, []),
    wxPanel:setSizer(Panel, MainSizer),      
% Menubar:
    MenuBar = wxMenuBar:new(),
    File = wxMenu:new(),
    Help = wxMenu:new(),
    Preferences = wxMenu:new(),
% Some stuff in menu:
    wxMenu:append(Help, ?ABOUT, "About Pear Talk"),
    wxMenu:append(File, 3, "Preferences\tCtrl-P"),
    wxMenu:append(File, ?SEND, "Send\tCtrl-S"),	
    wxMenuBar:append(MenuBar, File, "&File"),	
    wxMenuBar:append(MenuBar, Help, "&Help"),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "Welcome to Pear Talk!"),
    wxMenuBar:enable(MenuBar, 3, false),
    wxMenuBar:enable(MenuBar,?ABOUT, false),
    wxMenuBar:enable(MenuBar,?SEND, false),
% create two listeners
    wxFrame:connect(Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),
    wxFrame:show(Frame),
% the return value
    {Server, Frame, TextCtrl, TextCtrl2}.
 
loop(State) ->
    {Server, Frame, TextCtrl, TextCtrl2}  = State,
    receive
        #wx{event=#wxClose{}} ->
	    wxWindow:destroy(Frame), 
	    ok;  % exit the loop
	
    	#wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
	    wxWindow:destroy(Frame),
	    ok; 

	#wx{id = 104, event=#wxCommand{type = command_button_clicked} } ->	      
	    Str = "Send a reminder to your mailbox.",		
	    TED = wxTextEntryDialog:new(Frame,Str,[{value, "Email"}]),	
	    case wxDialog:showModal(TED) of		
		?wxID_OK ->
		    chat ! {login, reminder, wxTextEntryDialog:getValue(TED)};
		?wxID_CANCEL ->
		    cancel
	    end,
	    wxDialog:destroy(TED),
	    loop(State);

	#wx{id = 103, event=#wxCommand{type = command_button_clicked} } ->		%
	    spawn(reg_frame,start,[]),
	    loop(State);

    	#wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
            TextCtrl_val = wxTextCtrl:getValue(TextCtrl),
            TextCtrl2_val = wxTextCtrl:getValue(TextCtrl2),
            chat ! {login, {TextCtrl_val, TextCtrl2_val}},
	    wxWindow:destroy(Frame), 
	    ok; 
	
	Msg ->
	    loop(State)
    end.
