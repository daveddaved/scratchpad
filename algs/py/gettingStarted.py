class Classy(object):
    def __init__(self):
        self.items = []
        self.dict = {"tophat": 2, "bowtie": 4, "monocle": 5}

    def addItem(self, item):
        if not item in self.items:
          self.items.append(item)

    def printItems(self):
        print(self.items)
        for item in self.items:
            print(item)

    def getClassiness(self):
        total = 0
        for item in self.items:
            total += self.dict.get(item, 0)
        return total


        # Test cases
me = Classy()

# Should be 0
print(me.getClassiness())

me.addItem("tophat")
# Should be 2
print(me.getClassiness())

me.addItem("bowtie")
me.addItem("jacket")
me.addItem("monocle")
print(me.items)
# Should be 11
print (me.getClassiness())

me.addItem("bowtie")
# Should be 15
print(me.getClassiness())