#!/usr/bin/env python

# see 

#TODO:
#Get the files from that other website, play with them. If they don't already do the following, then:
#Make this move pdf into right direcotry
#Add annotations to a .org with same name in same file, under heading Annotations (* or **, haven't decided, base it on stuff from above)
#Make it do something similar to the R script with a bib file.....

#Figure out a way to get the notes to have a newline every so many characters, but only at a word break
#But not if it's a link (how check?)

def get_annotations(path,note_format='plain',org_indent='   ',fill_column=70):
    # Based on code from Steve Powell, found at
    # (https://gist.github.com/stevepowell99/e1e389a57ea9a2bcb988).
    # See his blog post
    # http://socialdatablog.com/extract-pdf-annotations.html for more
    # details
    
    #note_format, one of:
    #   plain -- no special formatting, each annotation on separate line
    #   wrap -- annotation wrapped so that each has a new line after fill_column characters
    #   org -- for .org notes file, wrapped after fill_column characters, each line starts with org_indent
    
    import poppler, os.path, os, time, datetime, re

    assert note_format in ['plain','wrap','org'],"Don't know how to format annotations with note_format %s"%note_format
    
    notes=''	
    # print("Found PDF:..."+fullpath)
    # print "Size: ... "+os.stat(fullpath).st_size
    path = "file://%s"%path
    try:
        doc = poppler.document_new_from_file(path, None)
    except:
        print "some pdf problem"
    else:
        pages = [doc.get_page(i) for i in range(doc.get_n_pages())]
        for page_no, page in enumerate(pages):
            items = [i.annot.get_contents() for i in page.get_annot_mapping()]
            items = [i for i in items if i]
            for j in items:
                #Every so often, we'll have an annotation with (1 or
                #more) newlines in it; we want to get rid of them. If
                #a line ends with a letter or number then it's the end
                #of a sentence, so add a period there.
                j = re.subn("([a-zA-Z0-9])\n",r"\1. ",j)[0].replace("\n"," ")
                if note_format=='plain':
                    notes= notes+'%s (page %s)\n\n' % (j,page_no + 1)
                elif note_format=='wrap':
                    j = col_wrap('%s (page %s)'%(j,page_no+1),fill_column)
                    notes=notes+'%s\n\n'%j
                elif note_format == 'org':
                    j = col_wrap('%s (page %s)'%(j,page_no+1),fill_column,org_indent)
                    notes=notes+'%s%s\n\n'%(org_indent,j)                    
        return notes

def col_wrap(text,fill_col,org_indent=''):
    text = text.split(' ')
    length = 0
    for idx,i in enumerate(text):
        if length+len(i) > fill_col:
            text[idx] = i+"\n%s"%org_indent
            length=0
        else:
            length+=len(i)
    text = ' '.join(text).replace("\n ","\n")
    return text
    
