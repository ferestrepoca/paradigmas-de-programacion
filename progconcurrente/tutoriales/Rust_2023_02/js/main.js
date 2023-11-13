const logos = [
    'aws.png',
    'cloudflare.png',
    'dropbox.png',
    'gcp.jpg',
    'microsoft.png',
    'mozilla.png',
]

const logoContainer = document.getElementById('companies-uses-rust')


logos.forEach(logo => {
    const logoDiv = document.createElement('div')
    logoDiv.classList.add('col-12')
    logoDiv.classList.add('col-md-2')
    const img = document.createElement('img')
    img.src = `./images/clients/${logo}`
    img.alt = logo
    img.style.maxWidth = '10em'


    logoDiv.appendChild(img)
    logoContainer.appendChild(logoDiv)
})