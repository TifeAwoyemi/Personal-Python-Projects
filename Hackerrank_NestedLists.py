# -*- coding: utf-8 -*-
"""
Created on Tue Mar 25 00:47:56 2025

@author: bawoyemi

Given the names and grades for each student in a class of  students, store them 
in a nested list and print the name(s) of any student(s) having the second 
lowest grade.

Note: If there are multiple students with the second lowest grade, order their 
names alphabetically and print each name on a new line.

Constraints:
    
2 <= N <= 5

There will always be one or more students having the second lowest grade.

"""
records = []
ind_records = []
scores_list = []
N = int(input())

while N < 2 or N > 5:
    N = int(input(''))

for _ in range(N):
    name = input()
    score = float(input())
    scores_list.append(score)
    ind_record = [name, score]
    records.append(ind_record)

records = sorted(records)   
scores_list = sorted(scores_list)
for i in range(len(scores_list)):
    if scores_list[i] == scores_list[i + 1]:
        continue
    second_lowest_grade = scores_list[i + 1]
    break

for i in range(len(records)):
    if records[i][1] == second_lowest_grade:
        print(records[i][0])