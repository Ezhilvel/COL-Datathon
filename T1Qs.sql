select * from T1;



select * from T1Qs
where year <> '2018'
order by recordnumber, current_quarter;

Drop table T1Q_domain;
Create table T1Q_domain as
select recordnumber, cast(Pay as int) Pay, cast(Q_Flag as int) Q_Flag, cast(current_quarter as int) current_quarter
from T1Qs
order by 1;

select A.*, B.* from T1Q_domain;


select A.recordnumber, A.current_quarter, B.current_quarter, B.Pay - A.Pay
from T1Q_domain A join T1Q_domain B 
on A.Recordnumber = b.recordnumber and A.current_quarter+1 = B.current_quarter
order by 1,2;

----------------------------------------------------------------
--new file
----------------------------------------------------------------

Create table T1_calc_Q1 as
select *, Q1Payments as Pay, Q1 as Q_Flag, Q1C as Current_quarter, diff1 as quarter_inc from T1_Calc;

Create table T1_calc_Q2 as
select *, Q2Payments as Pay, Q2 as Q_Flag, Q2C as Current_quarter, diff2 as quarter_inc from T1_Calc;

Create table T1_calc_Q3 as
select *, Q3Payments as Pay, Q3 as Q_Flag, Q3C as Current_quarter, diff3 as quarter_inc from T1_Calc;

--Drop table T1Q_master;
------------------------
create table T1Q_master as
select *, 1 quarter from T1_calc_Q1 union 
select *, 2 quarter from T1_calc_Q2 union 
select *, 3 quarter from T1_calc_Q3 union 
select *, 4 quarter from T1_calc_Q4  ;

---------------------------------------------
--master created
---------------------------------------------

Drop table T1Q_master_train_FT;
Create table T1Q_master_train_FT as
select *, quarter_inc/pay Perc_inc from T1Q_master
where year <> 2018 and employmenttype = "Full Time" and Current_quarter <> 0;

select count(*) from T1Q_master_train_FT;

Create table TQ_master_train_FT as
select * from T1Q_master_train_FT 
where current_quarter <> 1;

select count(*) from T1Q_master;

select count(distinct departmenttitle),  count(distinct (case when instr(departmenttitle,"(") = 0  then departmenttitle else 
substr(departmenttitle,0,instr(departmenttitle,"(")-1) end )) from raw_data;

select count(distinct departmenttitle) from Raw_data;
select distinct BenefitsPlan from TQ_master_train_FT;


select  a.employmenttype, count(*) from Raw_data_2018 a
join payroll_2018 b on a.recordnumber = b.recordnumber
where year  = '2018' group by 1
;

select * from payroll_2018;

Create table avg_pay_per_hour as
select recordnumber, avg(HourlyorEventRate)  from raw_data_2017
group by 1;

Create table 



