# Test ACStor v2 

1. Create aks cluster 

```
az aks create \
  --resource-group acstorv2testwei \
  --name acstorv2testwei \
  --nodepool-name np01 \
  --node-count 3 \
  --enable-addons monitoring \
  --generate-ssh-keys \
  --node-vm-size Standard_L8s_v3 \
```