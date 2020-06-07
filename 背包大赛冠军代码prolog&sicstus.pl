:- use_module(library(lists)).
:- use_module(library(samsort)).
:-use_module(library(clpfd)). 

main:-      
       File1='d:/workspace/beibao/src/data9',
       File2='d:/workspace/beibao/src/data9result',
       open(File1, read, Str),       
       read_line(Str,Line1),
       getlineinfo(Line1,Len,Weight),
       number_codes(L1,Len),
       number_codes(W1,Weight),
       readdata(Str,[],List,0,L1),
       close(Str),       
       samsort(List, L2),      
       getvwlist(L2,[],Valuelist,[],Weightlist,[],Numlist),              
       getmaxnum(Weightlist,W1,0,Num,0,Weight2),
       %原谅我这样写if,妮妮 
       if(Weight2>=W1,
          Num1 is Num-1,
          Num1 = Num),       
       getlistvalue(Valuelist,Num1,0,Valuesum),       
       write(Valuesum),nl,       
       %获得前Num1的总重量
       sumtill(Weightlist,Num1,0,Weight3,0),
       write(Weight3),nl,       
       write('clp'),nl,
       %从最后获得一个空间，与clp空间是30，前后各取15
       Num21 is Num1-14,   
       Num22 is Num1+15,       
       %获得保留下来的这段list
       Num31 is Num21-1,
       splitnumlist(Numlist,Num31,[],Numlist2,0),
       reverse(Numlist2,Numlist3),
       sumtill(Valuelist,Num31,0,Value3,0),
       splitlist(Weightlist,Num21,Num22,1,[],Clpweightlist),
       splitlist(Valuelist,Num21,Num22,1,[],Clpvaluelist), 
       splitlist(Numlist,Num21,Num22,1,[],Clpnumlist),
       reverse(Clpvaluelist,Clpvaluelist1),
       reverse(Clpweightlist,Clpweightlist1),
       reverse(Clpnumlist,Clpnumlist1),       
       %获得1-num21的总重量
       sumtill(Weightlist,Num21,1,Weight21,0),       
       Clpweight is W1-Weight21,     
       %clp,优化空间取30，再大会比较慢
       knapsack(Result,Value2,30,Clpweight,Clpvaluelist1,Clpweightlist1),       
       write('knap ok'),nl,
       getclplist(Clpnumlist1,Result,[],Clpnumlist2),
       append(Numlist3,Clpnumlist2,Resultlist),
       Totalvalue is Value3+Value2,
       sort(Resultlist,Resultlist1),
       %l1是总行数
       saveresult(File2,Resultlist1,Totalvalue,L1).     
%clp
knapsack(Result,Cost,Len,Wlimit,Vlist,Wlist):- 
                 length(Result,Len),
                 domain(Result,0,1), 
                 scalar_product(Wlist,Result,#=<,Wlimit),
                 scalar_product(Vlist,Result,#=,Cost),
                 labeling([ff,down,maximize(Cost)],Result).
%切分列表       
getclplist([],[],Cplnumlist,Cplnumlist).
getclplist([H1|T1],[H2|T2],Tem,Cplnumlist):-
        if(H2=1,
           append(Tem,[H1],Tem1),
           Tem1=Tem),
        getclplist(T1,T2,Tem1,Cplnumlist).

splitlist(_,_,Endnum,Num,Resultlist,Resultlist):-
        Num>Endnum,
        !.
splitlist([_|T],Startnum,Endnum,Num,Temlist,Resultlist):-
        Num<Startnum,
        Num1 is Num +1,
        !,
        splitlist(T,Startnum,Endnum,Num1,Temlist,Resultlist).
splitlist([H|T],Startnum,Endnum,Num,Temlist,Resultlist):-
        Num>=Startnum,
        Num=<Endnum,
        Temlist1=[H|Temlist],
        Num1 is Num+1,
        !,
        splitlist(T,Startnum,Endnum,Num1,Temlist1,Resultlist).
%保存结果文件
saveresult(File,List,Value,Linenum):-              
        open(File, write, Str2),
        write(Str2,Value),
        write(Str2,'\n'),        
        save1(Str2,List,Linenum,0),                
        close(Str2).
%保存结果文件
save1(_,_,Linenum,Linenum).
save1(Str2,[],Linenum,Tem):-        
        write(Str2,'0 '),  
        Tem1 is Tem+1,
        write(Tem),nl,
        !,
        save1(Str2,[],Linenum,Tem1).
save1(Str2,[H|T],Linenum,Tem):-
        Tem>H,
        write(Str2,'0 '),  
        Tem1 is Tem+1,
        write(Tem),nl,
        !,
        save1(Str2,[H|T],Linenum,Tem1).
save1(Str2,[H|T],Linenum,H):-
        write(Str2,'1 '),
        Line is H+1,
        write(H),nl,
        !,
        save1(Str2,T,Linenum,Line).
save1(Str2,[H|T],Linenum,Tem):-
        Tem<H,
        write(Str2,'0 '),  
        Tem1 is Tem+1,
        write(Tem),nl,
        !,
        save1(Str2,[H|T],Linenum,Tem1).

splitnumlist(_,Num1,Numlist1,Numlist1,Num1).
splitnumlist([H|T],Num1,Teml,Numlist1,Temn):-
        Teml1=[H|Teml],
        Temn1 is Temn+1, 
        !,
        splitnumlist(T,Num1,Teml1,Numlist1,Temn1).  
%两个位置之间的累加
sumtill(_,Num,Num,Weight,Weight).
sumtill([H|T],Num,Tem1,Weight,Tem2):-
        Tem3 is Tem1+1,
        Tem4 is Tem2+H,
        sumtill(T,Num,Tem3,Weight,Tem4).

getlistvalue([],_,Valuesum,Valuesum).
getlistvalue(_,0,Valuesum,Valuesum).
getlistvalue([H|T],Num1,Tem,Valuesum):-
        Tem1 is Tem+H,
        Num2 is Num1-1,
        getlistvalue(T,Num2,Tem1,Valuesum).

getmaxnum([],_,Num,Num,Weight,Weight).
getmaxnum(_,Wmax,Num,Num,Weight,Weight):-
        Weight>=Wmax,!. 
getmaxnum([H|T],Wmax,Tem1,Num,Temw,Weight):-
        Temw1 is Temw + H,
        Tem2 is Tem1+ 1,
        !,
        getmaxnum(T,Wmax,Tem2,Num,Temw1,Weight).
%从（X,V,W，N）中分别取出每个的列表
getvwlist([],Valuelist,Valuelist,Weightlist,Weightlist,Numlist,Numlist):-!.
getvwlist([(_,V,W,N)|T],Tem1,Valuelist,Tem2,Weightlist,Tem5,Numlist):-
        Tem3=[V|Tem1],
        Tem4=[W|Tem2],
        Tem6=[N|Tem5],
        !,
        getvwlist(T,Tem3,Valuelist,Tem4,Weightlist,Tem6,Numlist).
%读取文件并解析
readdata(_,List,List,Maxnum,Maxnum):-!.
readdata(Str,Tem,List,Num,Maxnum):-
       Num1 is Num+1,       
       read_line(Str,Line1),      
       getlineinfo(Line1,Value,Weight),       
       number_codes(V,Value),
       number_codes(W,Weight),       
       X is V/W,
       Tem1=[(X,V,W,Num)|Tem],
       write(Num),nl,
       !,
       readdata(Str,Tem1,List,Num1,Maxnum).

getlineinfo(Line,Value,Weight):-
        findblank(Line,[],Value,Weight).

findblank([32|T],V,V,T):-!.
findblank([H|T],Tem1,Value,Weight):-
        append(Tem1,[H],Tem3),!,
        findblank(T,Tem3,Value,Weight).
        