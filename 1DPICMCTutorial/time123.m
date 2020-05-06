
t=timer;
t.StartDelay=1;%relay  1S to begin
t.Executionmode='fixedRate';%use circle to do 
t.Period=1;
t.TasksToExecute=1000;
t.Timerfcn=@diaoyongtxt;
start(t)
