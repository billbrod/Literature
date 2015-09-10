mend_path = "/home/billbrod/Documents/Paper-annot/mend.sqlite"
bib_path = "/home/billbrod/Documents/Paper-annot/bibliography.bib"
paper_dir = "/home/billbrod/Documents/Paper-annot"

org_format="""#+STARTUP: showall
* {todo} {title} \t\t\t\t\t {tags}
  {read_entry}
  ADDED: <{date}>

** Keywords
   {kws}

** Notes
   {notes}

** Annotations
   {annotations}

** Links
   PDF: [[file:{pdf_path}]]
   BibTex: [[file:{bib_path}]]
   Notes: [[file:{org_path}]]
"""

def main(fill_column=70):
    import bibtexparser,shutil,os,re
    from lit_update import get_annotations,col_wrap
    import numpy as np
    import pandas as pd

    global mend_path,bib_path,paper_dir,org_format
    
    if paper_dir[-1]=='/':
        paper_dir=paper_dir[:-1]
    
    df,notes = parse_mend_db(mend_path)
    
    with open(bib_path) as f:
        bib_db = bibtexparser.loads(f.read())

    for bib in bib_db.entries:
        bib_id = bib['ID']
        print('For key %s'%bib_id)
        if bib_id not in df.citationKey.values:
            print('No match found for id %s, checking title %s'%(bib_id,bib['title']))
            mend_entry = df[df.documentName == bib['title']]
        else:
            mend_entry = df[df.citationKey == bib_id]
        assert not mend_entry.empty,'No entry found for id %s or title %s'%(bib_id,bib['title'])
        #Necessary to make this a Series, which behaves nicer
        mend_entry = mend_entry.squeeze()
        #Need to change path to get the actual one
        old_path = bib['file'].replace(":home","/home").replace(':pdf','')
        annot_path = paper_dir+'/Unsorted/'+os.path.splitext(os.path.basename(old_path))[0]+'-annotated.pdf'
        new_path = paper_dir+'/{bib_id}/{bib_id}.pdf'.format(bib_id=bib_id)
        bib_new_path = os.path.splitext(new_path)[0]+'.bib'
        org_path = os.path.splitext(new_path)[0]+'.org'
        os.makedirs(os.path.split(new_path)[0])
        print('Checking for annotated file at %s'%annot_path)
        if os.path.isfile(annot_path):
            print("Found annotated file")
            shutil.copyfile(annot_path,new_path)
            # os.renames(annot_path,new_path)
            annotations = "\n"+get_annotations(annot_path,note_format='org',org_indent=re.search('Annotations\n( *){annotations}',org_format).groups()[0],fill_column=fill_column)
        else:
            print("No annotated file found")
            shutil.copyfile(old_path,new_path)
            # os.renames(old_path,new_path)
            annotations=""
        #Don't want any spaces in the tags. 
        #Also want to iterate through tags and folder if they're lists, not if they're empty
        tmp = [i.replace(' ','') for i in {True:[],False:mend_entry.tags}.get(mend_entry.tags is np.nan)+{True:[],False:mend_entry.folder}.get(mend_entry.folder is np.nan)]
        tags = ":"
        for i in tmp:
            tags+="%s:"%i
        if len(tags)==1:
            tags = ""
        print('Tags are %s'%tags)
        tmp = mend_entry["keywords"]
        keywords = ""
        if tmp is not np.nan:
            for kw in tmp:
                keywords+=kw+"\n%s"%re.search('Keywords\n( *){kws}',org_format).groups()[0]
        tmp = mend_entry["headers"]
        headers = ""
        if tmp is not np.nan:
            tmp = tmp.split("\n  \n")
            for idx,t in enumerate(tmp):
                t = re.subn(" *\n *","\n",t)[0].replace("\n"," ")
                tmp[idx] = col_wrap(t,fill_column)
            headers = "\n\n".join(tmp)
            headers = headers.replace("\n","\n%s"%re.search('Notes\n( *){notes}',org_format).groups()[0])
        if mend_entry["dateAccessed"]:
            read_entry = "CLOSED: [%s]"%mend_entry["dateAccessed"]
        else:
            read_entry=""
        org_file = org_format.format(title=bib['title'],tags=tags,date=mend_entry['date_added'],read_entry=read_entry,annotations=annotations,notes=headers,pdf_path=new_path,bib_path=bib_new_path,todo={True:'TODO',False:'DONE'}.get(not mend_entry["dateAccessed"]),org_path=org_path,kws=keywords) 
        with open(org_path,'w') as f:
            f.write(org_file)
        bib['file'] = new_path
        bib_save = bibtexparser.bibdatabase.BibDatabase()
        bib_save.entries = [bib]
        with open(bib_new_path,'w') as f:
            bibtexparser.dump(bib_save,f)
        print('')
        #Get id
        #Try to find it in mendeley db, if not look for thing with similar title
        #Check for title-annotated in Unsorted folder
        #  copy it to new folder
        #  extract annotations? or use those from notes? Probably just use from notes, already done that work. Oh but there might be some on the file that aren't Mendeley's...
        #  Copy annotations + stuff in df to a .org file
        #  Save bib entry

    
    #Copy over bib, create master org by concatenating all orgs
        
    
def parse_mend_db(path):
    import sqlite3 as lite
    import pandas as pd
    import numpy as np
    import html2text,re,datetime

    con = lite.connect(path)
    cur = con.cursor()

    cur.execute("SELECT id, title FROM Documents")
    documents = cur.fetchall()
    documents = dict(documents)
    
    cur.execute("SELECT id, name FROM Folders")
    folders = cur.fetchall()
    folders= dict(folders)

    cur.execute("SELECT id, documentId, page, note, createdTime, modifiedTime FROM FileNotes")
    notes = cur.fetchall()

    cur.execute("SELECT id, added, citationKey, dateAccessed FROM Documents")
    date_added = cur.fetchall()
    date_added = np.array(date_added)
    
    cur.execute("SELECT documentId, folderId FROM DocumentFolders")
    doc_folders = cur.fetchall()
    doc_folders = np.array(doc_folders)
                   
    cur.execute("SELECT documentId, text FROM DocumentNotes")
    headers = cur.fetchall()
    headers = np.array(headers)
    
    cur.execute("SELECT documentId, tag FROM DocumentTags")
    tags = cur.fetchall()
    tags = np.array(tags)
    
    cur.execute("SELECT documentId, keyword FROM DocumentKeywords")
    keywords = cur.fetchall()
    keywords = np.array(keywords)
    
    notes = pd.DataFrame(data=notes, columns=["id", "documentId", "page", "note", "createdTime", "modifiedTime"])

    df = pd.DataFrame(data=date_added,index=date_added[:,0],columns=['documentName','date_added','citationKey','dateAccessed'])
    df.date_added = df.date_added.map(lambda x: datetime.date.fromtimestamp(x/1000).strftime("%Y-%m-%d"))

    headers = pd.DataFrame(data=headers[:,1],index=headers[:,0],columns=['header_note'])
    headers.index = headers.index.astype('int64')
    #Need to do this to get rid of that html-style markup
    headers.header_note = headers.header_note.map(lambda x: html2text.html2text(expandEntities(x)))
    #For some reason, Mendeley every once in a while made my headers
    #have way too many newlines, so we use this to reduce them (don't
    #think I have every personally made three newlines, so this should
    #only grab those that are a problem.).
    headers.header_note = headers.header_note.map(lambda x: x.replace("\n  \n  \n",""))
    #This finds if anywhere in this header we found a url (starting
    #with http and ending with a space) that had a newline (\n) in
    #it. It will replace any such url it finds by removing the \n but
    #I think it can only do one \n per url
    headers.header_note = headers.header_note.map(lambda x: re.subn("(http.*)\n(.*) ",r"\1\2",x)[0])
    
    #For tags and keywords, we want to get all the values for one
    #document into one row, so we turn them into a list.
    tmp = pd.DataFrame(data=tags[:,1],index=tags[:,0],columns=['tag'])
    tags = pd.DataFrame()
    for i in tmp.index:
        #The isinstance(t,basestring) here checks if the value is a
        #string or unicode, because in that case, there's only one
        #value and we want all of it. If that's not the case, then
        #it's a list or array and so we want just the first value
        tags.loc[i,'tag'] = [t[0] if not isinstance(t,basestring) else t for t in tmp.loc[i].values]
    tags.index = tags.index.astype('int64')
        
    tmp = pd.DataFrame(data=keywords[:,1],index=keywords[:,0],columns=['keyword'])
    keywords = pd.DataFrame()
    for i in tmp.index:
        keywords.loc[i,'keyword'] = [kw[0] if not isinstance(kw,basestring) else kw for kw in tmp.loc[i].values]
    keywords.index = keywords.index.astype('int64')
        
    tmp = pd.DataFrame(data=doc_folders[:,1],index=doc_folders[:,0],columns=['folder'])
    tmp.folder = tmp.folder.map(folders)
    doc_folders = pd.DataFrame()
    for i in tmp.index:
        doc_folders.loc[i,'folder'] = [fold[0] if not isinstance(fold,basestring) else fold for fold in tmp.loc[i].values]

    df['headers'] = headers
    df['tags'] = tags
    df['keywords'] = keywords
    df['folder'] = doc_folders
    
    notes["documentName"] = notes.documentId.map(documents)
    df["documentName"] = df.documentName.map(documents)
    
    return df,notes

def expandEntities(text):
    """from old version of HTML2Text: Converts HTML to clean and readable
    plain text. Found at www.aaronsw.com/2002/html2text/html2text-1.0.py

    For some reason, new version (installed through pip) wasn't doing
    this."""
    __author__ = "Aaron Swartz, based on code by Aaron Swartz and Lars Pind"
    __copyright__ = "(C) 2002 Aaron Swartz. GNU GPL 2"
    import re
    
    text = text.replace("&lt;", "<")
    text = text.replace("&gt;", ">")
    text = text.replace("&quot;", '"')
    text = text.replace("&ob;", "{")
    text = text.replace("&cb;", "}")
    text = text.replace("&middot;", "*")
    text = re.sub("&[rl]squo;", "'", text)
    text = re.sub("&[rl]dquo;", '"', text)
    text = re.sub("&([aeiou])(grave|acute|circ|tilde|uml|ring);", lambda m: m.groups(1)[0], text)
    text = re.sub(r'&#(\d+);', intEnt, text)
    text = re.sub(r'&#[Xx](\w+);', xEnt, text)
    text = re.sub("&(#169|copy);", "(C)", text)
    text = re.sub("&mdash;", "--", text)
    return text

def intEnt(m):
    """from old version of HTML2Text: Converts HTML to clean and readable
    plain text. Found at www.aaronsw.com/2002/html2text/html2text-1.0.py"""
    __author__ = "Aaron Swartz, based on code by Aaron Swartz and Lars Pind"
    __copyright__ = "(C) 2002 Aaron Swartz. GNU GPL 2"
    m = int(m.groups(1)[0])
    return unichr(m).encode('utf-8')

def xEnt(m):
    """from old version of HTML2Text: Converts HTML to clean and readable
    plain text. Found at www.aaronsw.com/2002/html2text/html2text-1.0.py"""
    __author__ = "Aaron Swartz, based on code by Aaron Swartz and Lars Pind"
    __copyright__ = "(C) 2002 Aaron Swartz. GNU GPL 2"
    m = int(m.groups(1)[0], 16)
    return unichr(m).encode('utf-8')

