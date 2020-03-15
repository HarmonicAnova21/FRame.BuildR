#Object List:
#             1.  Group
#             2.  Difference Set
#             3.  Frame


#Class:  Group
#Slots: -Group Elements
#       -Modulus

setClass("Group",slots=list(group = 'integer',modulus='integer'))

#Class:  Difference Set
#Slots: -Difference Set
#       -Group

setClass("Difference Set",slots=list(set = 'integer', group ='Group'))

#Class:  Frame
#Slots: -Matrix
#       -Dimension
#       -Size

setClass("Frame",slots=list(matrix = 'matrix',dimension = 'integer',size = 'integer'))

