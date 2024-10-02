import hrequests
import json
import time
import traceback
from pathlib import Path
from bs4 import BeautifulSoup

def scrape_ptg_threads(output_folder, start_page, urls_to_collect=100):
    #create the output folder if it doesn't exist
    folder = Path(output_folder)
    folder.mkdir(parents=True, exist_ok=True)
    
    ptg_threads = []
    
    #load existing thread URLs from the JSON file (if it exists)
    output_file = folder / "ptg_threads.json"
    if output_file.exists():
        with output_file.open("r", encoding="utf-8") as f:
            ptg_threads = json.load(f)
    
    #start scraping from the specified start page
    page_num = start_page

    #loop through pages of /pol/ to find /ptg/ thread URLs
    while len(ptg_threads) < (urls_to_collect + len(ptg_threads)):
        url = f"https://archived.moe/pol/page/{page_num}/"
        print(f"Scraping /pol/ page {page_num}: {url}")
        
        #send the request using hrequests
        try:
            response = hrequests.get(url, browser="chrome")
        except hrequests.exceptions.ClientException as e:
            print(f"ClientException while requesting page {page_num}: {traceback.format_exc()}")
            break

        #if the request was not successful, break the loop
        if response.status_code != 200:
            print(f"HTTP error {response.status_code} while scraping {url}")
            break

        #parse the page with BeautifulSoup
        soup = BeautifulSoup(response.text, 'html.parser')

        #find all thread stubs which contain thread information
        thread_stubs = soup.select('.thread.stub, .clearfix.thread')
        if not thread_stubs:
            print(f"No threads found on {url}")
            break

        #search for "/ptg/" in the thread titles and extract the thread URL
        for thread in thread_stubs:
            thread_title_tag = thread.select_one('.post_title')
            if thread_title_tag:
                thread_title = thread_title_tag.get_text(strip=True)
                if '/ptg/' in thread_title:
                    #extract the thread number from the "data-thread-num" attribute
                    thread_num = thread.get('data-thread-num')
                    if thread_num:
                        thread_url = f"https://archived.moe/pol/thread/{thread_num}"
                        if thread_url not in ptg_threads:
                            print(f"Found /ptg/ thread: {thread_url}")
                            ptg_threads.append(thread_url)

                            #save updated thread URLs to the JSON file
                            with output_file.open("w", encoding="utf-8") as f:
                                json.dump(ptg_threads, f, indent=2, ensure_ascii=False)
                            print(f"/ptg/ thread URL saved. Total URLs: {len(ptg_threads)}")

                            #stop the loop if we have collected 100 additional URLs
                            if len(ptg_threads) >= urls_to_collect + len(ptg_threads):
                                print(f"Scraping completed. {urls_to_collect} more /ptg/ thread URLs found.")
                                return

        #rate limiter to avoid bot detection
        print("Waiting 5 seconds before the next request...")  # <-- rate limiter
        time.sleep(5)  # wait 5 seconds before next scrape

        #move to the next page
        page_num += 1

#continue from page x to gather 100 more /ptg/ thread URLs
scrape_ptg_threads(output_folder="./ptg_threads_output", start_page=2600, urls_to_collect=100)
