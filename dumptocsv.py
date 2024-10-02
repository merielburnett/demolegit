import pandas as pd
import json
import re

#making function to clean post text by adding a space after ">>number" to preserve replies
def clean_post_text(text):
    if not text:
        return text
    #regularized expression to find occurrences like >>number and add a space after the number
    cleaned_text = re.sub(r'(>>\d+)(\S)', r'\1 \2', text)
    return cleaned_text

#load JSON data
with open('./scraped_thread_data.json', 'r', encoding='utf-8') as f:
    data = json.load(f)

#clean the post_text for each post in the data
for post in data:
    post['post_text'] = clean_post_text(post['post_text'])

#convert to pandas df
df = pd.DataFrame(data)

#save to CSV
df.to_csv('./scraped_thread_data.csv', index=False)

print("JSON cleaned and saved to CSV.")
