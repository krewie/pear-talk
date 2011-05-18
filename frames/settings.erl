-module(settings).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
 
start() ->
    State = make_window(),
    ok.
 
make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Settings", [{size,{250, 150}}]),
    Panel  = wxPanel:new(Frame),
 
	Choices = ["Default","Decorative","Serif","Handwriting","Sans-serif",
			   "Fixed pitch","Teletype"],
	C1001 = wxChoice:new(Panel, 1001,[{pos, {15, 5}},{choices, Choices}]),
	wxChoice:setColumns(C1001, [{n, 6}]),

    B101  = wxButton:new(Panel, 101, [{pos, {100, 30}},{label, "&Save"}]),
    B102  = wxButton:new(Panel, ?wxID_EXIT, [{pos, {15, 30}},{label, "E&xit"}]),
    
    T = wxIcon:new("Smiley.bmp",[]),
    A = wxIconBundle:new(),
    wxIconBundle:addIcon(A,T),
    
    
    wxFrame:show(Frame),
 
    wxFrame:connect( Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),
 
%% the return value, which is stored in State
    {Frame, C1001}.