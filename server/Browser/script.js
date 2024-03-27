async function getAddresses() {
    if (!window.cardano || !window.cardano.eternl) {
        alert ('Eternl wallet extension is not found. Please make sure it is installed and enabled.');
        console.log ('Eternl wallet extension is not found.');
        return }
    try {
        const eternl = await window.cardano.eternl.enable();
        const usedAddresses = await eternl.getUsedAddresses();
        const changeAddress = await eternl.getChangeAddress();
        if (!changeAddress) { throw new Error('Failed to retrieve the change address.') }
        if (!usedAddresses) { throw new Error('Failed to retrieve the used addresses.') }
        if (usedAddresses.length === 0) { Error('No used addresses found in the wallet.') }
        const response  = await fetch ('/submit-addresses',
                        { method    : 'POST'
                        , headers   : {'Content-Type': 'application/x-www-form-urlencoded'}
                        , body      : `usedAddresses=${encodeURIComponent(usedAddresses.join(','))}&changeAddress=${encodeURIComponent(changeAddress)}`
                        });
        if (!response.ok) { throw new Error('Failed to submit addresses to the server.') };
        console.log('Used Addresses submitted:' , usedAddresses );
        console.log('Change Address submitted:' , changeAddress );
        showAlert ('Addresses submitted successfully!' );
    } catch (error) { console.error ('Error:', error) ; alert (`Error: ${error.message}`) }
}

async function signTransaction() {
    if (!window.cardano || !window.cardano.eternl) {
        alert ('Eternl wallet extension is not found. Please make sure it is installed and enabled.');
        return }
    try {
        const eternl        = await window.cardano.eternl.enable();
        const response1     = await fetch ('/get-unsigned-transaction');
        const unsignedTx    = await response1.text();
        if (!unsignedTx) { alert('No unsigned transaction received.'); return }
        console.log ('Received unsigned transaction:', unsignedTx);
        const witnessSet    = await eternl.signTx (unsignedTx, true);
        const response2     = await fetch ('/submit-signed-witness',
                            { method    : 'POST'
                            , headers   : {'Content-Type': 'application/x-www-form-urlencoded'}
                            , body      : `witness=${encodeURIComponent(witnessSet)}`
                            });
        if (!response2.ok) { throw new Error ('Failed to submit signed transaction to the server.') };
        console.log ('Signed transaction submitted:', witnessSet );
        showAlert ('Transaction signed successfully!');
    } catch (error) { console.error ('Error signing transaction:', error); alert (`Error signing transaction: ${error.message}`) }
}

function showAlert(message) {
    document.querySelector('.custom-alert-message').textContent = message;
    document.getElementById('customAlertBox').style.display = 'flex' }

function closeAlert() {
    document.getElementById('customAlertBox').style.display = 'none' }

document.getElementById("fetchAddressesBtn").addEventListener("click", getAddresses);
document.getElementById("signTransactionBtn").addEventListener("click", signTransaction);