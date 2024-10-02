import hrequests
import json
import time as time_module
import traceback
from pathlib import Path
from bs4 import BeautifulSoup

def scrape_thread_data(json_file_path, output_file):
    #load thread URLs from the JSON file
    with open(json_file_path, 'r', encoding='utf-8') as json_file:
        thread_urls = json.load(json_file)
    
    #initialize an empty list to hold all post data
    all_posts = []
    
    #load existing data from the JSON file (if it exists)
    output_file_path = Path(output_file)
    if output_file_path.exists():
        with output_file_path.open("r", encoding="utf-8") as f:
            all_posts = json.load(f)
    
    #loop through each thread URL to scrape data
    for thread_url in thread_urls:
        thread_id = thread_url.split('/')[-1]  #get thread ID from the URL
        print(f"Scraping thread {thread_id}: {thread_url}")
        
        try:
            response = hrequests.get(thread_url, browser="chrome")
        except hrequests.exceptions.ClientException as e:
            print(f"ClientException while requesting thread {thread_id}: {traceback.format_exc()}")
            continue

        #if the request was not successful, skip this thread
        if response.status_code != 200:
            print(f"HTTP error {response.status_code} while scraping {thread_url}")
            continue

        #parse the page with BeautifulSoup
        soup = BeautifulSoup(response.text, 'html.parser')

        #find all post data using the provided CSS selectors
        post_wrappers = soup.select('.post_wrapper')
        for post_wrapper in post_wrappers:
            try:
                #extract post ID
                post_id_tag = post_wrapper.select_one('.post_data a+ a')
                post_id = post_id_tag.get_text(strip=True) if post_id_tag else None

                #extract post text
                post_text_tag = post_wrapper.select_one('.text')
                post_text = post_text_tag.get_text(strip=True) if post_text_tag else None

                #extract time of post
                time_tag = post_wrapper.select_one('time')
                post_time = time_tag['datetime'] if time_tag and 'datetime' in time_tag.attrs else None

                #extract flag (country information)
                flag_tag = post_wrapper.select_one('.flag')
                post_flag = flag_tag['title'] if flag_tag and 'title' in flag_tag.attrs else None

                #store post data in a dictionary
                post_data = {
                    'post_id': post_id,
                    'thread_id': thread_id,
                    'post_text': post_text,
                    'time': post_time,
                    'flag': post_flag
                }

                #add post data to the list of all posts
                all_posts.append(post_data)
                print(f"Scraped post {post_id} from thread {thread_id}")

            except Exception as e:
                print(f"Error processing post in thread {thread_id}: {traceback.format_exc()}")
        
        #save the data to JSON after scraping each thread
        with output_file_path.open("w", encoding="utf-8") as f:
            json.dump(all_posts, f, indent=2, ensure_ascii=False)

        #rate limiter 
        print("Waiting 6 seconds before the next request...")
        time_module.sleep(6)

    print(f"Scraping completed. Data saved to {output_file}")

#provide the path to the URLs JSON and the output JSON file
json_file_path = "C:/Users/your/path/goes/here/ptg_threads.json"
output_json_file = "./scraped_thread_data.json"

#kickoff the scrape
scrape_thread_data(json_file_path, output_file=output_json_file)
