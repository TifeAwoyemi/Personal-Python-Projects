# -*- coding: utf-8 -*-
"""
Created on Tue Mar 25 16:29:07 2025

@author: bawoyemi
"""

n = int(input())
while n < 2 or n > 10:
    n = int(input())
student_marks = {}
for _ in range(n):
    name, *line = input().split()
    scores = list(map(float, line))
    student_marks[name] = scores
query_name = input()
query_scores = student_marks[query_name]
average_score = (query_scores[0] + query_scores[1] + query_scores[2]) / 3
print(f"{average_score:.2f}")