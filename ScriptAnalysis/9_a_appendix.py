import os
from urllib.parse import quote

# Define paths
# Script is located in ScriptAnalysis/
# Output should be in Outcome/Appendix/
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(script_dir)
appendix_dir = os.path.join(project_root, "Outcome", "Appendix")
output_file = os.path.join(appendix_dir, "appendix.md")

# Appendix folders to process
folders_to_process = [
    f"Supplementary Appendix 1_{i}" for i in range(1, 6)
]

# Order list provided by user
ORDERED_DISEASES = [
    "Pneumonia", "Influenza", "Chickenpox", "Mumps", "Measles", "Scarlet fever",
    "Rubella", "Pertussis", "Leprosy", "Diphtheria", "Dengue fever", "Malaria",
    "Scrub Typhus", "Chikungunya", "Leptospirosis", "Melioidosis", "S. suis", "Zika virus",
    "Filariasis", "Trichinosis", "Brucellosis", "JE", "Leishmaniasis", "HFMD",
    "Amebiasis", "Shigellosis", "Typhoid", "Liver fluke", "HAV", "Paratyphoid",
    "Cholera", "HEV", "Enterovirus", "Gonorrhoea", "Syphilis", "HBV",
    "CA (HPV)", "Genital herpes", "Chancroid", "HCV", "HDV", "Other meningitis",
    "Encephalitis"
]

# Caption templates configuration
CAPTION_TEMPLATES = {
    "Supplementary Appendix 1_1": {
        "rules": [
            ("cases", "Comparison of observed and reconstructed (A) weekly and (B) monthly cases of {title}, 2020-2025."),
            ("deaths", "Comparison of observed and reconstructed monthly deaths of {title}, 2020-2025.")
        ],
        "default": "{title}."
    },
    "Supplementary Appendix 1_2": {
        "default": "Trends of {title}."
    },
    "Supplementary Appendix 1_3": {
        "default": "{title}."
    },
    "Supplementary Appendix 1_4": {
         "default": "Spatial distribution of {title} cases."
    },
    "Supplementary Appendix 1_5": {
         "default": "Detailed spatial analysis of {title}."
    }
}

# Appendix Section Titles
APPENDIX_SECTION_TITLES = {
    "Supplementary Appendix 1_1": "Part 1: Validation of data reconstruction",
    "Supplementary Appendix 1_2": "Part 2: Temporal trends of infectious diseases by group",
    "Supplementary Appendix 1_3": "Part 3: Disease ranking and age distribution validation",
    "Supplementary Appendix 1_4": "Part 4: Spatial distribution of incidence and mortality",
    "Supplementary Appendix 1_5": "Part 5: Detailed epidemiological verification for each disease"
}

# Create a lookup dictionary for faster sorting (title -> index)
DISEASE_ORDER_MAP = {name.lower(): i for i, name in enumerate(ORDERED_DISEASES)}

def get_caption(folder, subpath, title):
    config = CAPTION_TEMPLATES.get(folder, {})
    
    # Check specific rules based on subpath
    rules = config.get("rules", [])
    subpath_lower = subpath.lower()
    
    for keyword, template in rules:
        if keyword in subpath_lower:
            return template.format(title=title)
            
    # Fallback to default
    default_template = config.get("default", "{title}.")
    return default_template.format(title=title)

def get_sort_key(img_obj):
    """
    Returns a sort key tuple.
    Structure: (subpath_group, is_unknown_disease, order_index, title)
    1. subpath_group: To keep folders together.
    2. is_unknown_disease: 0 if in list, 1 if not. Puts known diseases first.
    3. order_index: The index from the user's list.
    4. title: Alphabetical fallback for items not in the list.
    """
    subpath = img_obj['subpath']
    if subpath == ".":
        subpath_group = ""
    else:
        subpath_group = subpath
    
    title = img_obj['title']
    title_lower = title.lower()
    
    # Check if exact match or lower case match
    if title_lower in DISEASE_ORDER_MAP:
        return (subpath_group, 0, DISEASE_ORDER_MAP[title_lower], "")
    else:
        return (subpath_group, 1, 0, title)

def generate_markdown():
    content = "# Supplementary Appendix\n\n"
    
    figure_count = 1
    
    for folder_name in folders_to_process:
        folder_path = os.path.join(appendix_dir, folder_name)
        
        if not os.path.exists(folder_path):
            print(f"Warning: {folder_path} does not exist.")
            continue
            
        # Format header: "Supplementary Appendix 1_1" -> "Supplementary Appendix 1.1"
        header_display = folder_name.replace("_", ".")
        
        # Add section title if available
        section_title = APPENDIX_SECTION_TITLES.get(folder_name)
        if section_title:
            header_display = section_title
        
        content += f"## {header_display}\n\n"
        
        # Traverse directory
        image_files = []
        for root, dirs, files in os.walk(folder_path):
            for file in files:
                if file.lower().endswith(('.png', '.jpg', '.jpeg')):
                    full_path = os.path.join(root, file)
                    rel_path = os.path.relpath(full_path, appendix_dir)
                    rel_path_fwd = rel_path.replace("\\", "/")
                    
                    title = os.path.splitext(file)[0]
                    subpath = os.path.relpath(root, folder_path)
                    
                    image_files.append({
                        'title': title,
                        'path': rel_path_fwd,
                        'subpath': subpath
                    })
        
        # Sort images
        image_files.sort(key=get_sort_key)
        
        current_subpath = None
        
        for img in image_files:
            # Check if we moved to a new subfolder
            img_subpath = img['subpath']
            if img_subpath == ".":
                img_subpath_display = None
            else:
                img_subpath_display = img_subpath
            
            if img_subpath_display != current_subpath:
                 if img_subpath_display:
                     sub_header = img_subpath_display.replace("\\", "/").capitalize()
                     content += f"### {sub_header}\n\n"
                 current_subpath = img_subpath_display
                
            # Add image
            encoded_path = quote(img['path'], safe='/')
            
            # Get caption text
            caption_text = get_caption(folder_name, img['subpath'], img['title'])
            
            # Format: Image first, then Caption below
            # Caption format: **Figure X. Title.** Caption text
            label_prefix = f"**Figure {figure_count}. {img['title']}.**"
            
            # Add to content
            content += f"![{label_prefix}]({encoded_path})\n\n"
            content += f"{label_prefix} {caption_text}\n\n"
            
            figure_count += 1
            
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print(f"Generated {output_file}")

if __name__ == "__main__":
    generate_markdown()
