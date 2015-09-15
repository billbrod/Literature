#!/usr/bin/env python

#For update:
# - For each bib entry in master bib, check since last master bib
#  update:
#  - If pdf updated: re-run get annotations
#  - If notes updated: update master notes
#  - If bib updated: update master bib

#This file only updates one-way: from local files to master files. For
#this reason, the master files are kept read-only. If this becomes too
#annoying, I may try to find a way to update both ways, but for now
#this works.

def update(fill_column=70):
    import os,stat,bibtexparser,time,re,shutil
    from lit_add import paper_dir,org_format
    
    paper_dir = os.path.expanduser(paper_dir)
    
    #Make it so we can write again...
    os.chmod(paper_dir+'/literature.bib',stat.S_IWUSR|stat.S_IREAD)
    os.chmod(paper_dir+'/literature.org',stat.S_IWUSR|stat.S_IREAD)
    
    shutil.copyfile(paper_dir+'/literature.bib',paper_dir+'/literature.backup.bib')
    shutil.copyfile(paper_dir+'/literature.org',paper_dir+'/literature.backup.org')
    os.chmod(paper_dir+'/literature.backup.bib',stat.S_IREAD|stat.S_IRGRP|stat.S_IROTH)
    os.chmod(paper_dir+'/literature.backup.org',stat.S_IREAD|stat.S_IRGRP|stat.S_IROTH)
    
    if paper_dir[-1]=='/':
        paper_dir=paper_dir[:-1]
    
    with open(paper_dir+'/literature.bib') as f:
        master_bib = bibtexparser.load(f)

    with open(paper_dir+'/literature.org') as f:
        master_org = f.read().decode('utf8')
    master_org = re.split('^[*] ([A-Z]+)',master_org,flags=re.MULTILINE)[1:]
    master_org = [(i,j,key_get(j)) for i,j in zip(master_org[::2],master_org[1::2])]
    
    to_remove = []
    modified_time = {'bib':os.path.getmtime(paper_dir+'/literature.bib'),'org':os.path.getmtime(paper_dir+'/literature.org')}
    for bib_idx,bib in enumerate(master_bib.entries):
        bib_id = bib['ID']
        if 'file' in bib: 
            dir_path = os.path.expanduser(os.path.split(bib['file'])[0])
        else:             
            dir_path = paper_dir+'/%s'%bib_id
        if not os.path.isdir(dir_path):
            #Then this has been removed and we get it out of here
            print('Directory for bib id %s not found, removing from master bib and org files...'%bib_id)
            master_org_idx = [bibid for (i,j,bibid) in master_org].index(bib_id)
            master_org.pop(master_org_idx)
            to_remove.append(bib)
            continue
        if os.path.isfile('%s/%s.pdf'%(dir_path,bib_id)) and os.path.getmtime('%s/%s.pdf'%(dir_path,bib_id)) > modified_time['org']:
            print('Pdf %s.pdf updated, extracting annotations'%bib_id)
            annots = get_annotations('%s/%s.pdf'%(dir_path,bib_id),'org',org_indent=re.search('Annotations *\n( *){annotations}',org_format).groups()[0],fill_column=fill_column)
            with open('%s/%s.org'%(dir_path,bib_id),'r+') as f:
                org_note = f.read().decode('utf8')
            org_hdr = org_note[:org_note.find('*')]
            org_note = re.split('^([*]+) (.*)',org_note,flags=re.MULTILINE)[1:]
            org_note = [(hdr_lvl,title,txt_body) for hdr_lvl,title,txt_body in zip(org_note[::3],org_note[1::3],org_note[2::3])]
            annotation_idx = [title.strip() for hdr_lvl,title,txt_body in org_note].index('Annotations')
            org_note[annotation_idx][2] = "\n\n"+annots+"\n\n"
            org_note = org_hdr+''.join([i+j+k for i,j,k in org_note])
            with open('%s/%s.org'%(dir_path,bib_id),'w') as f:
                f.write(org_note.encode('utf8'))
        if os.path.getmtime('%s/%s.org'%(dir_path,bib_id)) > modified_time['org']:
            print('Org file %s.org updated, copying new changes to master org'%bib_id)
            with open('%s/%s.org'%(dir_path,bib_id)) as f:
                org_note = f.read().decode('utf8')+'\n\n'
            #Drop everything before the first header (startup options, etc)
            org_note = org_note[org_note.find('*'):]
            org_note = re.split('^[*] ([A-Z]+)',org_note,flags=re.MULTILINE)[1:]
            org_note = (org_note[0],org_note[1],key_get(org_note[1]))
            master_org_idx = [bibid for (i,j,bibid) in master_org].index(bib_id)
            master_org[master_org_idx] = org_note
        if os.path.getmtime('%s/%s.bib'%(dir_path,bib_id)) > modified_time['bib']:
            print('Bib file %s.bib updated, copying new changes to master bib'%bib_id)
            with open('%s/%s.bib'%(dir_path,bib_id)) as f:
                bib_file = f.read().decode('utf8')
            master_bib.entries[bib_idx] = bibtexparser.loads(bib_file).entries[0]
                
    for bib in to_remove:
        master_bib.entries.remove(bib)
    with open(paper_dir+'/literature.bib','w') as f:
        master_bib_str = bibtexparser.dumps(master_bib)
        f.write(master_bib_str.encode('utf8'))
        
    master_org = '* '.join(['']+[i+j for i,j,k in master_org])
    with open(paper_dir+'/literature.org','w') as f:
        f.write(master_org.encode('utf8'))
            
    #We want these files to be read-only so that the only edits are to
    #the individual org and bib files. If this is too inconvenient,
    #may end up changing it.
    os.chmod(paper_dir+'/literature.bib',stat.S_IREAD|stat.S_IRGRP|stat.S_IROTH)
    os.chmod(paper_dir+'/literature.org',stat.S_IREAD|stat.S_IRGRP|stat.S_IROTH)
            

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

def key_get(org_contents):
    import re
    return re.findall(':BIBTEX-KEY: (.*) *\n',org_contents)[0]
    
if __name__ == '__main__':
    update()
