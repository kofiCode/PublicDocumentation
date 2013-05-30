# Define a function
def my_func(param1):
    "Optional documentation"
    print (param1)

# Main function (starting point of script)
def main():
    print ("Hello")
if __name__ == "__main__":
    main()

# System call 
from subprocess import call
call(["ls", "-l"])

# Output from system call:
output = s.check_output(["git", "status"]).decode("utf-8")
print (output)

# Open a terminal window: `urxvt` is my terminal program
import subprocess
child = subprocess.Popen("urxvt", shell=True)
# OR
process = subprocess.Popen(['ls', '-a'], stdout=subprocess.PIPE)
out, err = process.communicate()
print(out)

# User Input
var = raw_input("Enter something: ")
print ("you entered ", var)

# Lists
a = [66.25, 333, 333, 1, 1234.5]
print (a.count(333), a.count(66.25), a.count('x')) # ==> 2 1 0
a.insert(2, -1)
a.append(333)
print (a) # ==> [66.25, 333, -1, 333, 1, 1234.5, 333]
a.index(333) # ==> 1
a.remove(333)
print (a) # ==> [66.25, -1, 333, 1, 1234.5, 333]
a.reverse()
print (a) # ==> [333, 1234.5, 1, 333, -1, 66.25]
a.sort()
print (a) # ==> [-1, 1, 66.25, 333, 333, 1234.5]

# Looping
a = [66.25, 333, 333, 1, 1234.5]
for num in a:
    print (num)

# Switch/Case
if n == 0:
    print "You typed zero.\n"
elif n== 1 or n == 9 or n == 4:
    print "n is a perfect square\n"
elif n == 2:
    print "n is an even number\n"
elif  n== 3 or n == 5 or n == 7:
    print "n is a prime number\n"

# OR
options = {0 : zero,
                1 : sqr,
                4 : sqr,
                9 : sqr,
                2 : even,
                3 : prime,
                5 : prime,
                7 : prime,
}
 
def zero():
    print "You typed zero.\n"
 
def sqr():
    print "n is a perfect square\n"
 
def even():
    print "n is an even number\n"
 
def prime():
    print "n is a prime number\n"

# Dictionary/Hash
a = { "a": 123, "b": 456, "c": 789 }
for key, value in a.items():
    print ( "Key: " + key + ", Value: " + value )
print ( a["b"] )

# Strings
x = "Hello World!"
x[2:]   # 'llo World!'  Trim front
x[:2]   # 'He'          Keep front
x[:-2]  # 'Hello Worl'  Trim end
x[-2:]  # 'd!'          Keep end
x[2:-2] # 'llo Worl'    Trim front & end
