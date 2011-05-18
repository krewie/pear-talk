%% @author Staffan
%% @doc First draft of the chat window 
%% @copyright 2011 Peer-talk
-module(bild).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-define(SEND, 136).
-define(ABOUT , ?wxID_ABOUT).

%% @doc Init function for chat window. 
%% @spec chat_frame:start() -> no_return().
start() ->
    State = make_window(),
    %loop(State),
    ok.

%% @doc Sets up the chat window. Returns {Frame, T1001,  T1002} where T1001 is the text field that displays the conversation, T1002 the input field and Frame the "super object" in the window. 
%% @spec 
make_window() ->
	%% Create new wx-object, new window with panel and menubar...
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Pear Talk", [{size,{349, 550}}]),
    Panel = wxPanel:new(Frame, []),
  

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    BitmapSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[]),

    %% Create static texts

    Image = wxImage:new("icon.jpg", []),
    Bitmap = wxBitmap:new(wxImage:scale(Image, round(wxImage:getWidth(Image)*0.355),
											   round(wxImage:getHeight(Image)*0.25),
										[{quality, ?wxIMAGE_QUALITY_HIGH}])),
    StaticBitmap = wxStaticBitmap:new(Panel, 1, Bitmap),


    %% Add to sizers

    wxSizer:add(BitmapSizer, StaticBitmap, []),


    wxSizer:add(MainSizer, BitmapSizer, []),


    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:show(Frame).