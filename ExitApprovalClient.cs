using Msc.HRMS.Exit.Models;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.Configuration;
using System.Globalization;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using System.Web;
using System.Web.Script.Serialization;

namespace Msc.HRMS.Exit.UIServices
{
    public class ExitApprovalClient
    {
        /// <summary>
        /// Gets the leave request base URI.
        /// </summary>
        /// <value>The leave request base URI.</value>
        private string ApprovalRequestBaseUri
        {
            get
            {
                return Convert.ToString(ConfigurationManager.AppSettings["ExitRequestBaseUri"], CultureInfo.InstalledUICulture);
            }
        }

        public async Task UpdateStatus(ExitApproval exitApproval)
        {
            string getUri = this.ApprovalRequestBaseUri + "ExitRequest/UpdateStatus";

            using (var client = new HttpClient())
            using (HttpResponseMessage response = client.PostAsJsonAsync<ExitApproval>(getUri, exitApproval).Result)
            {
                using (HttpContent content = response.Content)
                {
                    string responseBody = await content.ReadAsStringAsync();
                }
                 response.EnsureSuccessStatusCode();
            }
        }

        public async Task<List<ApprovalDetail>> GetApprovalValues(string status, int id, int? organizationId)
        {
            List<ApprovalDetail> result = default(List<ApprovalDetail>);
            string getUri = this.ApprovalRequestBaseUri + "ExitRequest/GetApprovalList?status=" + status + "&id=" + id + "&organizationId=" + organizationId;

            using (var client = new HttpClient())
            using (HttpResponseMessage response = await client.GetAsync(getUri))            
            using (HttpContent content = response.Content)
            {
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                result = JsonConvert.DeserializeObject<List<ApprovalDetail>>(responseBody);
                return result;
            }
        }

        public async Task<List<Employee>> GetEmployee(int? organizationId)
        {
            List<Employee> result = default(List<Employee>);
            string getUri = this.ApprovalRequestBaseUri + "ExitRequest/GetEmployeeList?organizationId=" + organizationId;

            using (var client = new HttpClient())
            using (HttpResponseMessage response = await client.GetAsync(getUri))
            using (HttpContent content = response.Content)
            {
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                result = JsonConvert.DeserializeObject<List<Employee>>(responseBody);
                return result;
            }
        }

        public async Task<List<ApprovalDetail>> GetApplyFilterApprovalValues(ApprovalSearchViewModel ApprovalSearch, int userId,int? OrgId)
        {
            List<ApprovalDetail> result = default(List<ApprovalDetail>);
            string getUri = this.ApprovalRequestBaseUri + "ExitRequest/GetApprovalList?status=" + ApprovalSearch.Status + "&fromDate=" + ApprovalSearch.FromDate + "&toDate=" + ApprovalSearch.ToDate + "&employeeId=" + ApprovalSearch.EmployeeId + "&departmentId=" + ApprovalSearch.DepartmentId + "&id=" + userId + "&organizationId=" + OrgId;
            using (var client = new HttpClient())
            using (HttpResponseMessage response = await client.GetAsync(getUri))
            using (HttpContent content = response.Content)
            {
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                result = JsonConvert.DeserializeObject<List<ApprovalDetail>>(responseBody);
                return result;
            }
        }

       
    }
}