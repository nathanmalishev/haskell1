

@notes = ["A","B","C","D","E","G"]
@octaves = ["1","2","3"]
@possibleNote = []
@all = []

def inputGenerator
    @notes.each do |note|
        @octaves.each do |octave|
            @possibleNote.push( note + octave )
        end
    end
end


inputGenerator
print @possibleNote
 @possibleNote.permutation(3).to_a


def all
    for(int i=0; i<@possibleNote.length; i++)
        print "he"
    end
end
all
print @all